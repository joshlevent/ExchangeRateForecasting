# ------------------------------------------------------------------------------
#   Exchange Rate Forecasting
# ------------------------------------------------------------------------------
#   Applied Macroeconometrics, University of Neuchatel
#   Josh Levent, Julien Beaud, 2023
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# 0) Packages and functions that we need
# ------------------------------------------------------------------------------
library(readr)
library(httr)
library(xts)
library(dplyr)
# NOTE WELL: DPLYR and XTS require two different implementations of the lag() 
# function. Call these explicitly like this: dplyr::lag(df), stats::lag(xts)
library(tsbox)
library(purrr)
library(CADFtest)
library(ggplot2)
library(tidyr)
library(forecast)

# clean objects
rm(list=ls())

# Load user-defined commands and packages
source("UserPackages.R")

# Create an output folder in the current directory
mainDir = getwd()
outDir = makeOutDir(mainDir, "ERFOutput")

# ------------------------------------------------------------------------------
# 1) Data import, cleaning and preparation
# ------------------------------------------------------------------------------

# define locale for SNB imports
de_CH = locale(date_format = "%d.%m.%Y")

# 1.1 SARON
# Get data
url = "https://www.six-group.com/exchanges/downloads/indexdata/hsrron.csv"
response = GET(url)
data = content(response, "text")

# Save a cache of the data
timestamp = format(Sys.time(), "%Y%m%d-%H%M%S")
filename = paste0("../cache/SARON-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents and save to objects
df = read_delim(data, delim = "; ", skip = 4,
                 col_types = "Dd", col_names = c("Date", "Value"), 
                 locale = de_CH, col_select = c(1,2))

# fill missing dates, and then interpolate missing values
date_range <- seq(from = min(df$Date), to = max(df$Date), by = "day")
df <- data.frame(Date = date_range) %>% 
  left_join(df, by = "Date") %>%
  mutate(Value = na.approx(as.numeric(Value)))

# save data into objects
SARON = xts(df$Value, order.by = df$Date) 
SARONData = select(df, Date, Value)

# 1.2 SOFR
# Get data
url = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1319&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=SOFR&scale=left&cosd=2018-04-03&coed=2024-11-09&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-11-12&revision_date=2024-11-12&nd=2018-04-03"
response = GET(url)
data = content(response, "text")

# Save a cache of the data
timestamp = format(Sys.time(), "%Y%m%d-%H%M%S")
filename = paste0("../cache/SOFR-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents and save to objects
df = read_csv(data, skip = 1, col_types = "Dd", col_names = c("Date", "Value"), 
              na = c("", "ND", "."))

# fill missing dates, and then interpolate missing values
date_range <- seq(from = min(df$Date), to = max(df$Date), by = "day")
df <- data.frame(Date = date_range) %>% 
  left_join(df, by = "Date") %>%
  mutate(Value = na.approx(as.numeric(Value)))

# save data into objects
SOFR = xts(df$Value, order.by = df$Date) 
SOFRData = select(df, Date, Value)

# 1.3 Exchange Rate USD CHF
# Get data
url = "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2024&filetype=csv&label=include&layout=seriescolumn"
response = GET(url)
data = content(response, "text")

# Save a cache of the data
timestamp = format(Sys.time(), "%Y%m%d-%H%M%S")
filename = paste0("../cache/ER-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents
df = read_csv(data, skip = 6, col_types = "Dd", col_names = c("Date", "Value"), 
              na = c("", "ND", "."))

# trim leading and ending NAs
df <- na.trim(df)

# fill missing dates, and then interpolate missing values
date_range <- seq(from = min(df$Date), to = max(df$Date), by = "day")
df <- data.frame(Date = date_range) %>% 
  left_join(df, by = "Date") %>%
  mutate(Value = na.approx(as.numeric(Value)))

# save data into objects
ER = xts(df$Value, order.by = df$Date) 
ERData = select(df, Date, Value)

# Additional processing for ERData
ERData <- ERData %>%
  mutate(CHFUSD = 1 / Value,
         LogCHFUSD = log(CHFUSD) * 100,
         ForwardLogCHFUSD = lead(LogCHFUSD),
         LogDifferenceCHFUSD = ForwardLogCHFUSD - LogCHFUSD)

# 1.4 Swiss Policy Actions
# Get new data
url <- "https://data.snb.ch/json/table/getFile?fileId=2eb5650771935a870db45db19f098567acfe49cf8e9872727243fc1885238707&pageViewTime=20231127_172937&lang=en"
response <- GET(url)
data <- content(response, "text")

# Save a cache of the data
timestamp = format(Sys.time(), "%Y%m%d-%H%M%S")
filename = paste0("../cache/SwissPolicy1-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents
df <- read_delim(data, delim = ";", col_types = "Dcd", skip = 4, 
                 na = c("", "ND", "."), col_names = c("Date", "D0", "Value"))

# additional transformations to select only policy rate
df <- filter(df, D0 == "LZ")
df <- select(df, Date, Value)
df <- na.trim(df)

# save to xts object
SwissPolicyRate = xts(df$Value, order.by = df$Date) 

# transformations to select policy changes
SwissPolicyChange1 = ts_diff(SwissPolicyRate)
SwissPolicyChange1 = subset(SwissPolicyChange1, value != 0)

# Get old data
url <- "https://data.snb.ch/json/table/getFile?fileId=598a401ddfd66075bc2be3845d5f21e3ddf5e22d083ea30424af2e633e374778&pageViewTime=20231127_173150&lang=en"
response <- GET(url)
data <- content(response, "text")

# Save a cache of the data
timestamp = format(Sys.time(), "%Y%m%d-%H%M%S")
filename = paste0("../cache/SwissPolicy2-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents
df <- read_delim(data, delim = ";", col_types = "Dcd", skip = 4, 
                 na = c("", "ND", "."), col_names = c("Date", "D0", "Value"))

# additional transformations to select only policy rate
df <- df %>% filter(D0 == "UG")
df <- select(df, Date, Value)
df <- na.trim(df)

# save to xts object
SwissPolicyRate = xts(df$Value, order.by = df$Date) 

# transformations to select policy changes
SwissPolicyChange2 = ts_diff(SwissPolicyRate)
SwissPolicyChange2 = subset(SwissPolicyChange2, value != 0)

# combine old and new data
SwissPolicyChange = rbind(SwissPolicyChange1, SwissPolicyChange2)

# we're actually only interested in the dates here
# the rest of the processing to select the dates is still to be done

# 1.5 USA Policy Actions
# Get data
url = "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DFEDTARU&scale=left&cosd=2018-01-01&coed=2023-11-27&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily%2C%207-Day&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-11-27&revision_date=2023-11-27&nd=2008-12-16"
response = GET(url)
data = content(response, "text")

# Save a cache of the data
timestamp = format(Sys.time(), "%Y%m%d-%H%M%S")
filename = paste0("../cache/USPolicy-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents and save to objects
df = read_csv(data, skip = 1, col_types = "Dd", col_names = c("Date", "Value"))
USPolicyRate = xts(df$Value, order.by = df$Date) 

# transformations to select policy changes
USPolicyChange = ts_diff(USPolicyRate)
USPolicyChange = subset(USPolicyChange, value != 0)

# 1.6 Combining and trimming exchange rate and interest rate timeseries
# Select and rename relevant columns
SARONData <- SARONData %>% select(Date, SARON = Value)
SOFRData <- SOFRData %>% select(Date, SOFR = Value)
ERData <- ERData %>% select(Date, LogDifferenceCHFUSD, LogCHFUSD, CHFUSD)

# Combine dataframes
combinedData <- reduce(list(SARONData, SOFRData, ERData), full_join, by = "Date")

# Identify valid date range
first_valid_date <- combinedData %>% filter(!is.na(SARON) & !is.na(SOFR) & !is.na(LogDifferenceCHFUSD)) %>% summarize(first_date = min(Date)) %>% pull(first_date)
last_valid_date <- combinedData %>% filter(!is.na(SARON) & !is.na(SOFR) & !is.na(LogDifferenceCHFUSD)) %>% summarize(last_date = max(Date)) %>% pull(last_date)

# Trim data to valid range
trimmedData <- combinedData %>% filter(Date >= first_valid_date & Date <= last_valid_date)


# ------------------------------------------------------------------------------
# 2) Initial checks and tests
# ------------------------------------------------------------------------------

# checking whether the log difference approximation is suitable
max(trimmedData$LogDifferenceCHFUSD, na.rm = TRUE)
min(trimmedData$LogDifferenceCHFUSD, na.rm = TRUE)
# Yes, the absolute values of the change are below 4% so well approximated by the log value

# Calculate the interest rate differential
trimmedData <- trimmedData %>%
  mutate(InterestRateDiff = SARON - SOFR)

# split data into timeseries objects
IRdifferential = xts(trimmedData$InterestRateDiff, order.by = trimmedData$Date)
ERd = xts(trimmedData$LogDifferenceCHFUSD, order.by = trimmedData$Date)

# check for stationarity with u Root test
uRootER = CADFtest(ERd, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootER)
# this is stationary, good!
uRootIRd = CADFtest(IRdifferential, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootIRd)
# this is not stationary, bad!
# create a first difference variable
IRdifferential_d = IRdifferential - lag(IRdifferential)
# check the first difference for 
uRootFDIR = CADFtest(IRdifferential_d, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootFDIR)
# great, this is now stationary

# plot first difference of interest rate differential
ts_plot(IRdifferential_d)
p <- autoplot(IRdifferential_d)
p <- ggLayout(p) +
  labs(title = "First differences of Interest Rate differential between Switzerland and USA") +
  scale_y_continuous(breaks = seq(-3, 3, by = 0.5)) +
  theme(panel.grid.minor.x = element_line(colour = "black",linewidth=0.1,linetype="dotted"))
p
ggsave(paste(outDir, "First_diff_interest_rate_differential.png", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))

# plot first difference of log exchange rates
ts_plot(ERd)
p <- autoplot(ERd)
p <- ggLayout(p) +
  labs(title = "First differences of log exchange rate between Switzerland and USA") +
  scale_y_continuous(breaks = seq(-3, 3, by = 0.5)) +
  theme(panel.grid.minor.x = element_line(colour = "black",linewidth=0.1,linetype="dotted"))
p
ggsave(paste(outDir, "First_diff_log_exchange_rate.png", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))

# plot both the interest rates
InterestRatesLong <- pivot_longer(trimmedData, cols = c(SARON, SOFR), names_to = "id", values_to = "value")
InterestRatesLong = select(InterestRatesLong, Date, id, value)
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

p <- plotACF(ERd, 365)
p <- ggLayout(p) +
  labs(title = "Autocorrelation Function of first differences of log exchange rate")
p
ggsave(paste(outDir, "ACFExchangeRateDiff.png", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))
# there is some AC, but we expect some in 300 cases

plotACF(IRdifferential_d, 365)
ggsave(paste(outDir, "ACFInterestRateDiff.pdf", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))
# Similar to above, there is some autocorrelation.
# The strongly negative anti-correlation on day 1 is concerning. It suggests
# markets over-correct on news and then regress the day after...

p <- plotCCF(ERd, IRdifferential_d, lag.max = 365)
p <- ggLayout(p) +
  labs(title = "Cross Correlation Function of Interest Rate differential and Exchange Rate")
p
ggsave(paste(outDir, "CCF.pdf", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))
# Not more cross-correlation than could be expected by chance

# ------------------------------------------------------------------------------
# 3) Regression Analyses
# ------------------------------------------------------------------------------

# Perform simple regression analysis
result <- lm(ERd ~ IRdifferential_d)
summary(result)
# The p-value over 0.6 strongly implies no predictive power in the interest rates

# Check autocorrelation of residuals 
pdf("residuals.pdf", width = 11.7, height = 8.3)
checkresiduals(result)+theme_minimal()
dev.off()

# calculate error measures for empirical model
ME2 = mean(result$residuals)
T=length(result$residuals)
FEV2 = var(result$residuals)*(T-1)/T
MSFE2 = mean(result$residuals^2)
RMSFE2 = sqrt(MSFE2)
MAFE2 = mean(abs(result$residuals))



# Create forecast with theoretical model
trimmedData$TheoryForecast = dplyr::lag(trimmedData$InterestRateDiff/365 + trimmedData$LogCHFUSD)
trimmedData$TheoryCHFUSD = exp(trimmedData$TheoryForecast/100)
CHFUSD = xts(trimmedData$CHFUSD, order.by = trimmedData$Date)
TheoryCHFUSD = xts(trimmedData$TheoryCHFUSD, order.by = trimmedData$Date)
ts_plot(CHFUSD, TheoryCHFUSD)

# calculate error measures for theoretical model
trimmedData$TheoryError = trimmedData$LogCHFUSD - trimmedData$TheoryForecast
TheoryError = xts(trimmedData$TheoryError, order.by = trimmedData$Date)
TheoryError <- na.trim(TheoryError)
ts_plot(TheoryError)
ME1 = mean(TheoryError)
T=length(TheoryError)
FEV1 = var(TheoryError)*(T-1)/T
MSFE1 = mean(TheoryError^2)
RMSFE1 = sqrt(MSFE1)
MAFE1 = mean(abs(TheoryError))


# Fit ARIMA model for benchmark
arima_model <- auto.arima(ERd, stepwise=TRUE, approximation=TRUE, ic = c("aic"))
pdf("arima", width = 11.7, height = 8.3)
summary(arima_model)
checkresiduals(arima_model)
forecast <- forecast(arima_model, h = 365)
plot(forecast)
dev.off()

ME3 = mean(arima_model$residuals)
T=length(arima_model$residuals)
FEV3 = var(arima_model$residuals)*(T-1)/T
MSFE3 = mean(arima_model$residuals^2)
RMSFE3 = sqrt(MSFE3)
MAFE3 = mean(abs(arima_model$residuals))

# draw table of various error measures for all 3 models
Table = c(ME1^2/MSFE1, ME2^2/MSFE2, ME3^2/MSFE3)
Table = rbind(Table, c(FEV1/MSFE1, FEV2/MSFE2, FEV3/MSFE3))
Table = rbind(Table, c(RMSFE1, RMSFE2, RMSFE3))
Table = rbind(Table, c(MAFE1, MAFE2, MAFE3))
Table = round(Table, 2)
colnames(Table) = c("Theoretical", "Empirical", "AR(1)")
rownames(Table) = c("Share bias", "Share variance", "RMSFE", "MAFE")
Table

# ------------------------------------------------------------------------------
# 4) Rolling Forecast
# ------------------------------------------------------------------------------

# First we create a rolling AR(1) Forecast as a baseline
ER <- ts_span(ER, start = "2018-01-01")
horizon <- 30
ARFcst1 <- ER

ARFcst1[] <- NA
ARFcst14 <- ARFcst1
ARFcst30 <- ARFcst1

# Note that we can use maximum likelihood (ML) or conditional sum of squares 
# (CSS). MLE is slower but usually yields more stable results
myMethod = "CSS"
for(smpEnd in seq(as.Date(first_valid_date+700), as.Date(last_valid_date), by = "day")){  
  x = ts_ts(ts_span(ER, start = "2018-01-01", end = as.Date(smpEnd)))
  refit = Arima(x, order = c(1, 0, 0), include.constant= TRUE, method = myMethod)
  fcst = forecast(refit, h = horizon)

  
  ARFcst1[as.Date(index(ts_xts(fcst$mean))[1])] = fcst$mean[1]
  ARFcst14[as.Date(index(ts_xts(fcst$mean))[14])] = fcst$mean[14]
  ARFcst30[as.Date(index(ts_xts(fcst$mean))[30])] = fcst$mean[30]

}

ts_plot(
  `Exchange rate` = ER,
  `H = 1` = ARFcst1, 
  `H = 14` = ARFcst14,
  `H = 30` = ARFcst30, 
  title = "Exchange rate and AR(1) Forecasts",
  subtitle = ""
)

# ------------------------------------------------------------------------------
# TO-DO
# ------------------------------------------------------------------------------

#1 Create a benchmark and compare our model to the benchmark using 
    # the tools from App7

#2 Create a rolling forecast that takes in the data available at each date to 
    # predict the next dates (as in App7)

#3 Use the policy rate changes to restrict the time period to only those dates
    # within 2 days of a policy change to look for impacts created by policy

#4 There is a different data source for the SARON with the policy rate, I'm 
    # curious to check whether it shows exactly the same data
    # SARON2 <- df %>% filter(D0 == "SARON")

#5 What if we create a "naive" UIP forecast, that is not based on a regression
    # but just on the economic concept directly?

# 3 models: theoretical, estimated, AR(1)
# 2 samples: full, policy dates
# for both we compare errors
# only for full sample we do rolling forecast
