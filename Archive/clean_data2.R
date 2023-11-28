library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(purrr)

rm(list=ls())

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

# Save the cleaned data
write_csv(trimmedData, "cleaned_dataR.csv")
