# ExchangeRateForecasting
We are building a simple exchange rate forecasting tool for a class project.

Specifically we want to use uncovered interest rate parity to predict the exchange rate between pairs of currencies.

A short explanation of why this might work:

Interest rates vary from country to country. If we expected exchange rates to fluctuate randomly, then we could arbitrage the interest rates in different countries by moving money to the country with the highest interest rates, and then returning profits home. This arbitrage should move the exchange rate (and/or the interest rate) until it becomes unprofitable to conduct this arbitrage which implies that when interest rates in two countries are stable over time but different from one another, there must be a consistent appreciation of one currency relative to the other.



For this we need data on interest rates for various maturities, plus the exchange rate (spot prices only).


We will try to gather two types of samples for interest rates:  

- Shorter samples: SARON vs SOFR, overnight rates. We expect a short term impact on exchange rates (daily, weekly) - Data available for duration: 2018-2023  

  What is Saro :
  
  Saron is the average interest rate at which the Swiss National Bank (SNB) and commercial banks lend money to each other overnight. It is therefore very close to the SNB's key interest rate. SARON is calculated three times a day (12:00, 16:00, 18:00) by SIX and the daily reference rate is set at 18:00.
  
  All Saron Value per day (since 1999) : https://www.six-group.com/exchanges/indices/data_centre/swiss_reference_rates/reference_rates_en.html (very precise)
  
  What is SOFR : SOFR sets the rate at which banks can borrow cash from individuals or other banks overnight. The rate is collateralised by the US treasury securities market – these are bonds issued by the US government. It is established each business day around 8 a.m. by the Federal Reserve Bank of New York (around 13h in Schweizerreich).
  
  Swiss Francs to U.S. Dollar Spot Exchange Rate (open market price) from 04.01.1971 to 06.10.2023 : one dollar in xx CHF : 
  https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2023&filetype=csv&label=include&layout=seriescolumn


- Longer sample: monthly data, longer maturities (1 month, 3 month, 6 month, 12 month), long term impact on exchange rates (monthly, yearly) - Data available for duration: 2000 - 2023

  Saron per day, 1 month,3 mont, 6 month : https://data.snb.ch/en/topics/ziredev/cube/zirepo?fromDate=1999-01-01&toDate=2023-10-21&dimSel=D0(H0,H6,H7,H8,H1,H2,H3,H4,H5)
