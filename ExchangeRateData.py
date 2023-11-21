import pandas as pd
import requests
from io import StringIO
import numpy as np
url = "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2024&filetype=csv&label=include&layout=seriescolumn"
response = requests.get(url)
data = StringIO(response.content.decode('utf-8'))
ERData = pd.read_csv(data, skiprows=5, na_values="ND")
ERData.columns = ['Date', 'Value']
ERData['Date'] = pd.to_datetime(ERData['Date'])
date_range = pd.date_range(start=ERData['Date'].min(), end=ERData['Date'].max(), freq='D')
ERData = ERData.set_index('Date').reindex(date_range).rename_axis('Date').reset_index()
ERData['USDCHF'] = ERData['Value'].interpolate(method='linear')
ERData['CHFUSD'] = 1 / ERData['USDCHF']
ERData['LogCHFUSD'] = np.log(ERData['CHFUSD'])
ERData['LaggedLogCHFUSD'] = ERData['LogCHFUSD'].shift(1)
ERData['LogDifferenceCHFUSD'] = ERData['LogCHFUSD'] - ERData['LaggedLogCHFUSD']
