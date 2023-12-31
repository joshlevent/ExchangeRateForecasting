# Google working document

https://docs.google.com/document/d/1BoiwtZHhprqOUOmihyyuo-lJK93lbMU6fZf5fjdrcgw/edit?usp=sharing

# ExchangeRateForecasting
We are building a simple exchange rate forecasting tool for a class project.

Specifically we want to use uncovered interest rate parity to predict the exchange rate between pairs of currencies.

A short explanation of why this might work:

Interest rates vary from country to country. If we expected exchange rates to fluctuate randomly, then we could arbitrage the interest rates in different countries by moving money to the country with the highest interest rates, and then returning profits home. This arbitrage should move the exchange rate (and/or the interest rate) until it becomes unprofitable to conduct this arbitrage which implies that when interest rates in two countries are stable over time but different from one another, there must be a consistent appreciation of one currency relative to the other.



For this we need data on interest rates for various maturities, plus the exchange rate (spot prices only).


We will try to gather two types of samples for interest rates:  

- Shorter samples: SARON vs SOFR, overnight rates. We expect a short term impact on exchange rates (daily, weekly) - Data available for duration: 2018-2023  

  What is Saron :
  
  Saron is the average interest rate at which the Swiss National Bank (SNB) and commercial banks lend money to each other overnight. It is therefore very close to the SNB's key interest rate. SARON is calculated three times a day (12:00, 16:00, 18:00) by SIX and the daily reference rate is set at 18:00. SARON is continually calculated in real time since 6 pm the day before and published every 10 minutes. In addition, a fixing is conducted three times a day at 12pm (calculation from 6 pm before to 12 pm today), 4pm and 6pm. The 6pm fixing serves as a reference reading for derivative financial products and the valuation of financial assets. https://www.cler.ch/de/hypotheken-und-kredite/hypotheken/saron-hypothek-faq
  
  All Saron Value per day (since 1999) : https://www.six-group.com/exchanges/indices/data_centre/swiss_reference_rates/reference_rates_en.html (very precise)
  
  What is SOFR : SOFR sets the rate at which banks can borrow cash from individuals or other banks overnight. The rate is collateralised by the US treasury securities market – these are bonds issued by the US government. It is established each business day around 8 a.m. by the Federal Reserve Bank of New York (around 13h in Schweizerreich). The SOFR rate published on anyday represents the rate on repo transactions entered into on the previous business day and the date associated with each rate reflects the date of the underlying transactions rather than the date of publication : https://www.newyorkfed.org/medialibrary/Microsites/arrc/files/2021/users-guide-to-sofr2021-update.pdf
  
  Swiss Francs to U.S. Dollar Spot Exchange Rate (open market price) from 04.01.1971 to 06.10.2023 : one dollar in xx CHF (published at 4:15 p.m - 21h15 in Schweizerland): 
  https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2023&filetype=csv&label=include&layout=seriescolumn

  Worries: the day's exchange rate is published at 21h15 Swiss time, the SOFR at 13h Swiss time and the Saron at 12h, 16h and 18h Swiss time.

- Longer sample: monthly data, longer maturities (1 month, 3 month, 6 month, 12 month), long term impact on exchange rates (monthly, yearly) - Data available for duration: 2000 - 2023

  One year government bonds

  Swiss (since 1994) : https://www.investing.com/rates-bonds/switzerland-1-year-bond-yield-historical-data (see cs files uploaded)
  USA (since 2008) : https://www.investing.com/rates-bonds/u.s.-1-year-bond-yield-historical-data

  Not possible to use SOFT (per day), then it's possible to use this one (not sure) ? https://www.federalreserve.gov/releases/H15/default.htm

13.11.2023 : Teacher proposess AR(1) model based just on exchange rate (ER) with forecast model that uses the covered interest rate parity to predict ER 

We don't see effect because interested rate is expected

Kaufmann : Should we try to do a decomposition of the ER diff ?

Can we do a partial regression of just the peaks in the ER diff ?

Which result we present ?

How do we do the prediction using the exchange rate using the interest rate ?

Arima Model is thins kind of model suitable for daily data ? Ten days moving average ? What is big P ? Is Arima model pertinent ?
