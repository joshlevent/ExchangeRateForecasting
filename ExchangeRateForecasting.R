# ------------------------------------------------------------------------------
#   Exchange Rate Forecasting
# ------------------------------------------------------------------------------
#   Applied Macroeconometrics, University of Neuchatel
#   Josh Levent, Julien Beaud, 2023
# ------------------------------------------------------------------------------
#
#   Disclaimer: 
#         Some of the following code was generated, inspired and debugged
#         by ChatGPT. Below are ChatGPT transcripts of all interactions
#         related to this in reverse chronological order.
#
#         https://chat.openai.com/share/03be357b-67a0-4917-8b0c-6dc983f33d40
#         https://chat.openai.com/share/3c07f9b2-5317-4902-810c-0a5ee97c067d
#         https://chat.openai.com/share/794b0c9a-8e88-4eae-a863-7c77249abcc5
#         https://chat.openai.com/share/0d448d22-253f-46ae-8e69-77339fc7e4d1
#         https://chat.openai.com/share/89488cba-2a6c-49ba-8745-1d6399eb5a70
#         https://chat.openai.com/share/de512a26-ffab-4b43-9274-9422588c9cf1
#         https://chat.openai.com/share/b7da986f-807f-4630-8acb-2e399d7ae7d3
#         https://chat.openai.com/share/92baa853-f1a0-4e41-a4f7-4aee28c760b7
#         https://chat.openai.com/share/2895d604-02e5-4c4a-9f1f-115d8f6f1689
#         https://chat.openai.com/share/0506e655-2623-417f-b7fa-d56a0dd3d131
#         https://chat.openai.com/share/8b48f6b9-5e7e-4afe-8ad0-4d798a261063
#         https://chat.openai.com/share/8f5fff34-7da3-469e-a2bf-c6898bdd8153
#           
#         Aside from this, code was copied from Applications written by the
#         Professor, or written by hand.
#
# ------------------------------------------------------------------------------
#   0) Packages and functions that we need
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
library(lubridate)
library(knitr)
library(kableExtra)

# clean objects
rm(list=ls())

# Load user-defined commands and packages
source("UserPackages.R")

# Create an output folder in the current directory
mainDir <- getwd()
outDir <- makeOutDir(mainDir, "ERFOutput")

# ------------------------------------------------------------------------------
#   1) Data import, cleaning and preparation
# ------------------------------------------------------------------------------

# define locale for SNB imports
de_CH <- locale(date_format = "%d.%m.%Y")

# 1.1 SARON
# Get data
url <- "https://www.six-group.com/exchanges/downloads/indexdata/hsrron.csv"
response <- GET(url)
data <- content(response, "text")

# Save a cache of the data
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
filename <- paste0("../cache/SARON-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents and save to objects
df <- read_delim(data, delim = "; ", skip = 4,
                 col_types = "Dd", col_names = c("Date", "Value"), 
                 locale = de_CH, col_select = c(1,2))

# fill missing dates, and then interpolate missing values
date_range <- seq(from = min(df$Date), to = max(df$Date), by = "day")
df <- data.frame(Date = date_range) %>% 
  left_join(df, by = "Date") %>%
  mutate(Value = na.approx(as.numeric(Value)))

# save data into objects
SARON <- xts(df$Value, order.by = df$Date) 
SARONData <- select(df, Date, Value)

# 1.2 SOFR
# Get data
url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1319&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=SOFR&scale=left&cosd=2018-04-03&coed=2024-11-09&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-11-12&revision_date=2024-11-12&nd=2018-04-03"
response <- GET(url)
data <- content(response, "text")

# Save a cache of the data
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
filename <- paste0("../cache/SOFR-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents and save to objects
df <- read_csv(data, skip = 1, col_types = "Dd", col_names = c("Date", "Value"), 
              na = c("", "ND", "."))

# fill missing dates, and then interpolate missing values
date_range <- seq(from = min(df$Date), to = max(df$Date), by = "day")
df <- data.frame(Date = date_range) %>% 
  left_join(df, by = "Date") %>%
  mutate(Value = na.approx(as.numeric(Value)))

# save data into objects
SOFR <- xts(df$Value, order.by = df$Date) 
SOFRData <- select(df, Date, Value)

# 1.3 Exchange Rate USD CHF
# Get data
url <- "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2024&filetype=csv&label=include&layout=seriescolumn"
response <- GET(url)
data <- content(response, "text")

# Save a cache of the data
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
filename <- paste0("../cache/ER-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents
df <- read_csv(data, skip = 6, col_types = "Dd", col_names = c("Date", "Value"), 
              na = c("", "ND", "."))

# trim leading and ending NAs
df <- na.trim(df)

# fill missing dates, and then interpolate missing values
date_range <- seq(from = min(df$Date), to = max(df$Date), by = "day")
df <- data.frame(Date = date_range) %>% 
  left_join(df, by = "Date") %>%
  mutate(Value = na.approx(as.numeric(Value)))

# save data into objects
ER <- xts(df$Value, order.by = df$Date) 
ERData <- select(df, Date, Value)

# Additional processing for ERData
ERData <- ERData %>%
  mutate(CHFUSD = 1 / Value,
         LogCHFUSD = log(CHFUSD) * 100, # in percent
         ForwardLogCHFUSD = lead(LogCHFUSD), # lead is t+1
         LogDifferenceCHFUSD = ForwardLogCHFUSD - LogCHFUSD)

# 1.4 Swiss Policy Actions
# Get new data
url <- "https://data.snb.ch/json/table/getFile?fileId=2eb5650771935a870db45db19f098567acfe49cf8e9872727243fc1885238707&pageViewTime=20231127_172937&lang=en"
response <- GET(url)
data <- content(response, "text")

# Save a cache of the data
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
filename <- paste0("../cache/SwissPolicy1-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents
df <- read_delim(data, delim = ";", col_types = "Dcd", skip = 4, 
                 na = c("", "ND", "."), col_names = c("Date", "D0", "Value"))

# additional transformations to select only policy rate
df <- filter(df, D0 == "LZ")
df <- select(df, Date, Value)
df <- na.trim(df)

# save to xts object
SwissPolicyRate <- xts(df$Value, order.by = df$Date) 

# transformations to select policy changes
SwissPolicyChange1 <- ts_diff(SwissPolicyRate)
SwissPolicyChange1 <- subset(SwissPolicyChange1, value != 0)

# Get old data
url <- "https://data.snb.ch/json/table/getFile?fileId=598a401ddfd66075bc2be3845d5f21e3ddf5e22d083ea30424af2e633e374778&pageViewTime=20231127_173150&lang=en"
response <- GET(url)
data <- content(response, "text")

# Save a cache of the data
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
filename <- paste0("../cache/SwissPolicy2-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents
df <- read_delim(data, delim = ";", col_types = "Dcd", skip = 4, 
                 na = c("", "ND", "."), col_names = c("Date", "D0", "Value"))

# additional transformations to select only policy rate
df <- df %>% filter(D0 == "UG")
df <- select(df, Date, Value)
df <- na.trim(df)

# save to xts object
SwissPolicyRate <- xts(df$Value, order.by = df$Date) 

# transformations to select policy changes
SwissPolicyChange2 <- ts_diff(SwissPolicyRate)
SwissPolicyChange2 <- subset(SwissPolicyChange2, value != 0)

# combine old and new data
SwissPolicyChange <- rbind(SwissPolicyChange1, SwissPolicyChange2)

# 1.5 USA Policy Actions
# Get data
url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1318&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=DFEDTARU&scale=left&cosd=2018-01-01&coed=2023-11-27&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily%2C%207-Day&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2023-11-27&revision_date=2023-11-27&nd=2008-12-16"
response <- GET(url)
data <- content(response, "text")

# Save a cache of the data
timestamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
filename <- paste0("../cache/USPolicy-", timestamp, ".csv")
write(data, file = filename)

# parse the file contents and save to objects
df <- read_csv(data, skip = 1, col_types = "Dd", col_names = c("Date", "Value"))
USPolicyRate <- xts(df$Value, order.by = df$Date) 

# transformations to select policy changes
USPolicyChange <- ts_diff(USPolicyRate)
USPolicyChange <- subset(USPolicyChange, value != 0)

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
#   2) Initial checks and tests
# ------------------------------------------------------------------------------

# checking whether the log difference approximation is suitable
max(trimmedData$LogDifferenceCHFUSD, na.rm = TRUE)
min(trimmedData$LogDifferenceCHFUSD, na.rm = TRUE)
# Yes, the absolute values of the change are below 4% so well approximated by the log value

# Calculate the interest rate differential
# this is i^CH - i^US
trimmedData <- trimmedData %>%
  mutate(IRd = SARON - SOFR)

# split data into timeseries objects
IRd <- xts(trimmedData$IRd, order.by = trimmedData$Date)
ERd <- xts(trimmedData$LogDifferenceCHFUSD, order.by = trimmedData$Date)

# check for stationarity with u Root test
uRootER <- CADFtest(ERd, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootER)
# this is stationary, good!
uRootIRd <- CADFtest(IRd, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootIRd)
# this is not stationary, bad!
# create a first difference variable
# this is IRd_t - IRd_t-1 (lag 1)
IRdd <- IRd - stats::lag(IRd)
# check the first difference for 
uRootFDIR <- CADFtest(IRdd, max.lag.y = 10, type = "drift", criterion = "BIC")
summary(uRootFDIR)
# great, this is now stationary

# plot first difference of interest rate differential
ts_plot(IRdd)
p <- autoplot(IRdd)
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
InterestRatesLong <- select(InterestRatesLong, Date, id, value)
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

plotACF(IRdd, 365)
ggsave(paste(outDir, "ACFInterestRateDiff.pdf", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))
# Similar to above, there is some autocorrelation.
# The strongly negative anti-correlation on day 1 is concerning. It suggests
# markets over-correct on news and then regress the day after...

p <- plotCCF(ERd, IRdd, lag.max = 365)
p <- ggLayout(p) +
  labs(title = "Cross Correlation Function of Interest Rate differential and Exchange Rate")
p
ggsave(paste(outDir, "CCF.pdf", sep = "/"), plot = last_plot(), width = 21, height = 14.8, units = c("cm"))
# Not more cross-correlation than could be expected by chance

# ------------------------------------------------------------------------------
#   3) Regression Analyses
# ------------------------------------------------------------------------------

# Perform simple regression analysis
# THIS IS AN IN-SAMPLE TEST (BEST CASE)
result <- lm(ERd ~ IRdd)
summary(result)
# The p-value over 0.5 strongly implies no predictive power in the interest rates

# Check autocorrelation of residuals 
pdf("residuals.pdf", width = 11.7, height = 8.3)
checkresiduals(result)+theme_minimal()
dev.off()

# calculate error measures for empirical model
EmpiricalError = ts(result$residuals)
ME2 = mean(EmpiricalError)
T=length(EmpiricalError)
FEV2 = var(EmpiricalError)*(T-1)/T
MSFE2 = mean(EmpiricalError^2)
RMSFE2 = sqrt(MSFE2)
MAFE2 = mean(abs(EmpiricalError))


# Create forecast with theoretical model
trimmedData$TheoryForecast = dplyr::lag(trimmedData$IRd/365 + trimmedData$LogCHFUSD)
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
pdf("arima.pdf", width = 11.7, height = 8.3)
summary(arima_model)
checkresiduals(arima_model)
forecast <- forecast(arima_model, h = 365)
plot(forecast)
dev.off()

ARIMAError = arima_model$residuals
ARIMAError = ts_span(ARIMAError, end = "2067")
ME3 = mean(ARIMAError)
T=length(ARIMAError)
FEV3 = var(ARIMAError)*(T-1)/T
MSFE3 = mean(ARIMAError^2)
RMSFE3 = sqrt(MSFE3)
MAFE3 = mean(abs(ARIMAError))

# add random walk forecast, previous value
trimmedData$RWForecast = dplyr::lag(trimmedData$LogCHFUSD)
trimmedData$RWError = trimmedData$LogCHFUSD - trimmedData$RWForecast
RWError = ts(trimmedData$RWError)
RWError = na.trim(RWError)
ME4 = mean(RWError)
T=length(RWError)
FEV4 = var(RWError)*(T-1)/T
MSFE4 = mean(RWError^2)
RMSFE4 = sqrt(MSFE4)
MAFE4 = mean(abs(RWError))

# draw table of various error measures for all 3 models
Table = c(ME1^2/MSFE1, ME2^2/MSFE2, ME3^2/MSFE3, ME4^2/MSFE4)
Table = rbind(Table, c(FEV1/MSFE1, FEV2/MSFE2, FEV3/MSFE3, FEV4/MSFE4))
Table = rbind(Table, c(RMSFE1, RMSFE2, RMSFE3, RMSFE4))
Table = rbind(Table, c(MAFE1, MAFE2, MAFE3, MAFE4))
Table = round(Table, 2)
colnames(Table) = c("Theoretical", "Empirical", "AR(1)", "Random Walk")
rownames(Table) = c("Share bias", "Share variance", "RMSFE", "MAFE")
Table
save(Table, file = "main_error_table.RData")


# Diebold Mariano test
# a function allows us to test all combinations of a list
dm_test_pairs <- function(error_list) {
  if(is.null(names(error_list))) {
    stop("Please provide names for the error series in the list.")
  }
  n <- length(error_list)
  results_matrix <- matrix(NA, n, n, dimnames = list(names(error_list), names(error_list)))
  for (i in 1:(n-1)) {
    for (j in (i+1):n) {
      model1_error <- error_list[[i]]
      model2_error <- error_list[[j]]
      test_result <- dm.test(model1_error, model2_error, h = 1, power = 2)
      
      results_matrix[i, j] <- paste(round(test_result$statistic, 3), " (", round(test_result$p.value, 2), ")", sep = "")
      results_matrix[j, i] <- paste(round(test_result$statistic * -1, 3), " (", round(test_result$p.value, 2), ")", sep = "")
    }
  }
  return(results_matrix)
}

errors_list <- list(
  Theoretical = TheoryError,
  Empirical = EmpiricalError,
  "AR(1)"= ARIMAError,
  "Random Walk" = RWError
)

dm_test <- dm_test_pairs(errors_list)
dm_test
save(dm_test, file = "dm_test_table.RData")

# a p value of 0.89 for the DM test between the empirical model and the AR(1)
# model suggests that the two forecasts are not significantly different from
# one another

# The only significantly different models are the AR(1) and Theoretical Model
# and the Random Walk and AR(1) models. In both cases the AR(1) model is better
# The absolute values of the errors are very similar, therefore, likely this is
# a small difference in the error that is showing statistical significance
# because of the large sample

# ------------------------------------------------------------------------------
#   4) Rolling Forecast
# ------------------------------------------------------------------------------

#  we create a rolling AR(1) Forecast as a baseline
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
#   5) Narrow sample
# ------------------------------------------------------------------------------
# combine US and Swiss policy changes
AllPolicyChange = rbind(USPolicyChange,SwissPolicyChange)

# extract the dates from the time series object
dates <- as.Date(time(AllPolicyChange))
# restrict the range to our sample range
dates <- dates[dates >= first_valid_date & dates <= last_valid_date]

# Function to get dates 2 days before and after a given date
expand_dates <- function(date) {
  seq(from = date - 2, to = date + 2, by = "days")
}

# Apply the function to each date and create a vector of all relevant dates
expanded_dates <- unique(unlist(lapply(dates, expand_dates)))

filteredData <- trimmedData[trimmedData$Date %in% expanded_dates, ]

IRd_narrow = xts(filteredData$IRd, order.by = filteredData$Date) 
ERd_narrow = xts(filteredData$LogDifferenceCHFUSD, order.by = filteredData$Date)

result_narrow <- lm(ERd_narrow ~ IRd_narrow)
summary(result_narrow)
# p-value of 0.86 - not significant

# Fit ARIMA model
arima_model_narrow = Arima(ERd_narrow, order = c(1, 0, 0), include.constant= TRUE, method = "ML")
pdf("arima_narrow.pdf", width = 11.7, height = 8.3)
summary(arima_model_narrow)
checkresiduals(arima_model_narrow)
forecast <- forecast(arima_model_narrow, h = 365)
plot(forecast)
dev.off()

arima_narrow_error = arima_model_narrow$residuals
rw_narrow_error = ts(filteredData$RWError)
theory_narrow_error = ts(filteredData$TheoryError)
empirical_narrow_error = ts(result_narrow$residuals)

calculate_error_measures <- function(error_series_list) {
  # Check if the list has names
  if(is.null(names(error_series_list))) {
    stop("Please provide names for the error series in the list.")
  }
  
  # Initialize a list to store calculated measures for each error series
  measures_list <- list()
  
  for (error_series_name in names(error_series_list)) {
    error_series <- error_series_list[[error_series_name]]
    T <- length(error_series)
    
    ME <- mean(error_series)
    FEV <- var(error_series) * (T - 1) / T
    MSFE <- mean(error_series^2)
    RMSFE <- sqrt(MSFE)
    MAFE <- mean(abs(error_series))
    
    # Compute Share Bias and Share Variance
    ShareBias <- ME^2 / MSFE
    ShareVariance <- FEV / MSFE
    
    # Store the measures in the list
    measures_list[[error_series_name]] <- c(ShareBias, ShareVariance, RMSFE, MAFE)
  }
  
  # Convert the list to a data frame
  error_table <- do.call(cbind, measures_list)
  
  # Set column and row names
  rownames(error_table) <- c("Share bias", "Share variance", "RMSFE", "MAFE")
  
  # Round the table values
  return(round(error_table, 2))
}

error_series_list <- list(
  Theoretical = theory_narrow_error,
  Empirical = empirical_narrow_error,
  "AR(1)"= arima_narrow_error,
  "Random Walk" = rw_narrow_error
)

error_table <- calculate_error_measures(error_series_list)
print(error_table)
save(error_table, file = "narrow_error_table.RData")


narrow_dm_test <- dm_test_pairs(error_series_list)
narrow_dm_test
save(narrow_dm_test, file = "narrow_dm_test_table.RData")
