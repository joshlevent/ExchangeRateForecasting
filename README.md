# ExchangeRateForecasting
We are building a simple exchange rate forecasting tool for a class project.

Specifically we want to use uncovered interest rate parity to predict the exchange rate between pairs of currencies.

A short explanation of why this might work:

Interest rates vary from country to country. If we expected exchange rates to fluctuate randomly, then we could arbitrage the interest rates in different countries by moving money to the country with the highest interest rates, and then returning profits home. This arbitrage should move the exchange rate (and/or the interest rate) until it becomes unprofitable to conduct this arbitrage which implies that when interest rates in two countries are stable over time but different from one another, there must be a consistent appreciation of one currency relative to the other.



For this we need data on interest rates for various maturities, plus the exchange rate (spot prices only).


We will try to gather two types of samples for interest rates:  
Shorter samples: SARON vs SOFR, overnight rates. We expect a short term impact on exchange rates (daily, weekly) - Data available for duration: 2018-2023  
Longer sample: monthly data, longer maturities (1 month, 3 month, 6 month, 12 month), long term impact on exchange rates (monthly, yearly) - Data available for duration: 2000 - 2023
