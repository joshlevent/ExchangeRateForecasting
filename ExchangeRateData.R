library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)

# Define the URL
url <- "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2024&filetype=csv&label=include&layout=seriescolumn"

# Fetch the data
response <- GET(url)
data <- content(response, "text", encoding = "utf-8")
ERData <- read_csv(data, skip = 6, na = "ND", col_names = c('Date', 'Value'))

# Convert Date to Date format and create a sequence of dates
ERData$Date <- as.Date(ERData$Date)
date_range <- seq(from = min(ERData$Date), to = max(ERData$Date), by = "day")

# Join with the complete date sequence (like reindex in pandas)
ERData <- data.frame(Date = date_range) %>%
  left_join(ERData, by = "Date")

# Linear interpolation for missing values
ERData$Value <- na.approx(ERData$Value)

# Calculate USDCHF and CHFUSD
ERData$USDCHF <- ERData$Value
ERData$CHFUSD <- 1 / ERData$USDCHF

# Calculate LogCHFUSD and LaggedLogCHFUSD
ERData$LogCHFUSD <- log(ERData$CHFUSD)
ERData$LaggedLogCHFUSD <- lag(ERData$LogCHFUSD, 1)

# Calculate LogDifferenceCHFUSD
ERData$LogDifferenceCHFUSD <- ERData$LogCHFUSD - ERData$LaggedLogCHFUSD

write.csv(ERData, file = "ExchangeRateData.csv", row.names = FALSE)

