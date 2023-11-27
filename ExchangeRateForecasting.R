# ------------------------------------------------------------------------------
#   Exchange Rate Forecasting
# ------------------------------------------------------------------------------
#   Applied Macroeconometrics, University of Neuchatel
#   Josh Levent, Julien Beaud, 2023
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Packages that we need
library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(purrr)
library(lmtest)
library(forecast)
library(tseries)
library(tsbox)
library(ggplot2)

# clean objects
rm(list=ls())

# Load user-defined commands and packages
source("UserPackages.R")

# Here, it executes the function and creates the folder in the current directory
mainDir <- getwd()
outDir <- makeOutDir(mainDir, "/ERFOutput")

# Function to fetch and preprocess data
fetch_data <- function(url, skip_rows, delimiter, date_format) {
  response <- GET(url)
  data <- content(response, "text")
  df <- read_delim(data, delim = delimiter, skip = skip_rows, na = c("", "ND", "."), col_names = c("Date", "Value"))
  
  if (nrow(df) > 0) {
    df$Date <- as.Date(df$Date, format = date_format)
    date_range <- seq(from = min(df$Date), to = max(df$Date), by = "day")
    df <- data.frame(Date = date_range) %>% 
      left_join(df, by = "Date") %>%
      mutate(Value = na.approx(as.numeric(Value)))
  }
  
  return(df)
}

# Data URLs
SARONurl <- "https://www.six-group.com/exchanges/downloads/indexdata/hsrron.csv"
SOFRurl <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1319&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=SOFR&scale=left&cosd=2018-04-03&coed=2024-11-09&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-11-12&revision_date=2024-11-12&nd=2018-04-03"
ERurl <- "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2024&filetype=csv&label=include&layout=seriescolumn"

# Fetch data for each series
SARONData <- fetch_data(SARONurl, 4, ";", "%d.%m.%Y")
SOFRData <- fetch_data(SOFRurl, 1, ",", "%Y-%m-%d")
ERData <- fetch_data(ERurl, 6, ",", "%Y-%m-%d")

# Additional processing for ERData
ERData <- ERData %>%
  mutate(USDCHF = 1 / Value,
         LogCHFUSD = log(USDCHF),
         LaggedLogCHFUSD = lag(LogCHFUSD),
         LogDifferenceCHFUSD = LogCHFUSD - LaggedLogCHFUSD)

# Select relevant columns
SARONData <- SARONData %>% select(Date, SARON = Value)
SOFRData <- SOFRData %>% select(Date, SOFR = Value)
ERData <- ERData %>% select(Date, LogDifferenceCHFUSD)

# Combine datasets
combinedData <- reduce(list(SARONData, SOFRData, ERData), full_join, by = "Date")


# Identify valid date range
first_valid_date <- combinedData %>% filter(!is.na(SARON) & !is.na(SOFR) & !is.na(LogDifferenceCHFUSD)) %>% summarize(first_date = min(Date)) %>% pull(first_date)
last_valid_date <- combinedData %>% filter(!is.na(SARON) & !is.na(SOFR) & !is.na(LogDifferenceCHFUSD)) %>% summarize(last_date = max(Date)) %>% pull(last_date)

# Trim data to valid range
trimmedData <- combinedData %>% filter(Date >= first_valid_date & Date <= last_valid_date)

# Check for missing dates
date_range <- seq(from = min(trimmedData$Date), to = max(trimmedData$Date), by = "day")
missing_dates <- setdiff(date_range, trimmedData$Date)
if (length(missing_dates) == 0) {
  print("No missing dates in the series.")
} else {
  print(paste("Missing dates:", missing_dates))
}

# checking whether the log difference approximation is suitable
max(trimmedData$LogDifferenceCHFUSD, na.rm = TRUE)
min(trimmedData$LogDifferenceCHFUSD, na.rm = TRUE)
# Yes, the absolute values of the change are below 4% so well approximated by the log value

# Calculate the interest rate differential
trimmedData <- trimmedData %>%
  mutate(InterestRateDiff = SARON - SOFR)

# split data in two
ModelData <- trimmedData %>% select(Date, LogDifferenceCHFUSD, InterestRateDiff)
InterestRates <- trimmedData %>% select(Date, SARON, SOFR)

# unpivot the timeseries'
ModelDataLong <- ts_long(ModelData)
InterestRatesLong <- ts_long(InterestRates)

ts_plot(ModelDataLong)
p <- ggplot(ModelDataLong, aes(x = Date, y = value, color = id)) +
  geom_line()
p <- ggLayout(p) +
  labs(title = "Exchange rates and interest rate differences for Switzerland and the United States Over Time") +
  scale_color_manual(values = c("LogDifferenceCHFUSD" = "firebrick4", "InterestRateDiff" = "blue4"),
                     labels = c("LogDifferenceCHFUSD" = "1-day difference in log exchange rate", "InterestRateDiff" = "Interest Rate Difference")) +
  scale_y_continuous(breaks = seq(-6, 0, by = 0.5)) +
  theme(panel.grid.minor.x = element_line(colour = "black",linewidth=0.1,linetype="dotted"))
p
ggsave(paste(outDir, "ModelData.pdf", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))

ts_plot(InterestRatesLong)
p <- ggplot(InterestRatesLong, aes(x = Date, y = value, color = id)) +
  geom_line()
p <- ggLayout(p) +
  labs(title = "Swiss and US Interest Rates (Secured Overnight) Over Time") +
  scale_color_manual(values = c("SARON" = "firebrick4", "SOFR" = "blue4"),
                     labels = c("SARON" = "Switzerland", "SOFR" = "United States")) +
  scale_y_continuous(breaks = seq(-1, 5.5, by = 0.5)) +
  theme(panel.grid.minor.x = element_line(colour = "black",linewidth=0.1,linetype="dotted"))
p
ggsave(paste(outDir, "InterestRates.pdf", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))

plotACF(ModelData$LogDifferenceCHFUSD, 365)
ggsave(paste(outDir, "ACFExchangeRateDiff.pdf", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))

plotACF(ModelData$InterestRateDiff, 365)
ggsave(paste(outDir, "ACFInterestRateDiff.pdf", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))
# !!! This is highly autocorrelated, is this a problem???

p <- plotCCF(ts_ts(ModelData), ts_ts(ModelData$LogDifferenceCHFUSD), lag.max = 15)


# Perform simple regression analysis
result <- lm(LogDifferenceCHFUSD ~ InterestRateDiff, data = ModelData)
summary(result)

# Regression with some lags
result2 <- lm(LogDifferenceCHFUSD ~ InterestRateDiff + lag(InterestRateDiff) + lag(InterestRateDiff, 2) + lag(InterestRateDiff, 3) + lag(InterestRateDiff, 4), data = ModelData)
summary(result2)

# Durbin-Watson test for autocorrelation
dw_test <- dwtest(result)
print(dw_test)
# there is autocorrelation

# Check if the series is stationary
adf.test(trimmedData$LogDifferenceCHFUSD, k = 365)
# does NOT appear to be stationary (high p value means we cannot reject null 
# hypothesis that it is not stationary)
# Non-staionary-ness is not really an issue for ARIMA models though

log_diff_ts <- ts(trimmedData$LogDifferenceCHFUSD, frequency = 365)

season.test(log_diff_ts)

# Fit ARIMA model
# auto.arima automatically selects the best p, d, q parameters
arima_model <- auto.arima(log_diff_ts, stepwise=FALSE, approximation=FALSE)

# Open PDF document to save output
pdf("output.pdf", width = 11.7, height = 8.3)

# Summary of the model
summary(arima_model)

# Check model diagnostics
checkresiduals(arima_model)

# ARIMA forecasts
forecast <- forecast(arima_model, h = 365)
plot(forecast)

dev.off()
