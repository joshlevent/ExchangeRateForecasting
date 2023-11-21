import pandas as pd
import requests
from io import StringIO
import numpy as np

SARONurl = "https://www.six-group.com/exchanges/downloads/indexdata/hsrron.csv"
SARONresponse = requests.get(SARONurl)
SARONstring = StringIO(SARONresponse.content.decode('utf-8'))
SARONData = pd.read_csv(SARONstring, delimiter=';', skiprows=3)
SARONData = SARONData.iloc[:, :2]
SARONData.columns = ['Date', 'Value']
SARONData['Date'] = pd.to_datetime(SARONData['Date'], format='%d.%m.%Y')
SARONdate_range = pd.date_range(start=SARONData['Date'].min(), end=SARONData['Date'].max(), freq='D')
SARONData = SARONData.set_index('Date').reindex(SARONdate_range).rename_axis('Date').reset_index()
SARONData['SARON'] = SARONData['Value'].interpolate(method='linear')

SOFRurl = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1319&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=SOFR&scale=left&cosd=2018-04-03&coed=2024-11-09&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-11-12&revision_date=2024-11-12&nd=2018-04-03"
SOFRresponse = requests.get(SOFRurl)
SOFRstring = StringIO(SOFRresponse.content.decode('utf-8'))
SOFRData = pd.read_csv(SOFRstring, skiprows=0, na_values=".")
SOFRData.columns = ['Date', 'Value']
SOFRData['Date'] = pd.to_datetime(SOFRData['Date'])
SOFRdate_range = pd.date_range(start=SOFRData['Date'].min(), end=SOFRData['Date'].max(), freq='D')
SOFRData = SOFRData.set_index('Date').reindex(SOFRdate_range).rename_axis('Date').reset_index()
SOFRData['SOFR'] = SOFRData['Value'].interpolate(method='linear')

ERurl = "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2024&filetype=csv&label=include&layout=seriescolumn"
ERresponse = requests.get(ERurl)
ERstring = StringIO(ERresponse.content.decode('utf-8'))
ERData = pd.read_csv(ERstring, skiprows=5, na_values="ND")
ERData.columns = ['Date', 'Value']
ERData['Date'] = pd.to_datetime(ERData['Date'])
date_range = pd.date_range(start=ERData['Date'].min(), end=ERData['Date'].max(), freq='D')
ERData = ERData.set_index('Date').reindex(date_range).rename_axis('Date').reset_index()
ERData['USDCHF'] = ERData['Value'].interpolate(method='linear')
ERData['CHFUSD'] = 1 / ERData['USDCHF']
ERData['LogCHFUSD'] = np.log(ERData['CHFUSD'])
ERData['LaggedLogCHFUSD'] = ERData['LogCHFUSD'].shift(1)
ERData['LogDifferenceCHFUSD'] = ERData['LogCHFUSD'] - ERData['LaggedLogCHFUSD']

SARONData.set_index('Date', inplace=True)
SOFRData.set_index('Date', inplace=True)
ERData.set_index('Date', inplace=True)

combinedData = pd.merge(SARONData[['SARON']], SOFRData[['SOFR']], left_index=True, right_index=True, how='outer')
combinedData = pd.merge(combinedData, ERData[['LogDifferenceCHFUSD']], left_index=True, right_index=True, how='outer')

combinedData.reset_index(inplace=True)
trimmedData = combinedData.dropna(subset=['SARON', 'SOFR', 'LogDifferenceCHFUSD'])

trimmedData['Date'] = pd.to_datetime(trimmedData['Date'])

full_date_range = pd.date_range(start=trimmedData['Date'].min(), end=trimmedData['Date'].max(), freq='D')

missing_dates = full_date_range.difference(trimmedData['Date'])

if len(missing_dates) == 0:
    print("No missing dates in the series.")
else:
    print("Missing dates:", missing_dates)

trimmedData.to_csv("cleaned_data.csv", index=False)
