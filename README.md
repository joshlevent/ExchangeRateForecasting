# ExchangeRateForecasting
We are building a simple exchange rate forecasting tool for a class project.

Specifically we want to use uncovered interest rate parity to predict the exchange rate between pairs of currencies.

A short explanation of why this might work:

Interest rates vary from country to country. If we expected exchange rates to fluctuate randomly, then we could arbitrage the interest rates in different countries by moving money to the country with the highest interest rates, and then returning profits home. This arbitrage should move the exchange rate (and/or the interest rate) until it becomes unprofitable to conduct this arbitrage which implies that when interest rates in two countries are stable over time but different from one another, there must be a consistent appreciation of one currency relative to the other.



For this we need data on interest rates for various maturities, we will try daily, weekly?, 1 month, 3 month and 1 year rates, plus the exchange rate (spot prices only).
