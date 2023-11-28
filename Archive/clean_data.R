library(httr)
library(readr)
library(dplyr)
library(lubridate)
library(zoo)
library(purrr)

fetch_data <- function(url, skip_rows, delimiter = ",", column_names = c("Date", "Value"), date_format = NULL) {
  response <- GET(url)
  data <- content(response, "text")
  df <- read_delim(data, delim = delimiter, skip = skip_rows, na = c("", "ND", "."), col_names = column_names, col_types = cols(.default = col_character()))
  df$Date <- as.Date(df$Date, format = date_format)
  df
}

SARONurl <- "https://www.six-group.com/exchanges/downloads/indexdata/hsrron.csv"
SARONData <- fetch_data(SARONurl, skip_rows = 4, delimiter = ";", date_format = "%d.%m.%Y")
date_range <- seq(from = min(SARONData$Date), to = max(SARONData$Date), by = "day")
SARONData <- data.frame(Date = date_range) %>%
  left_join(SARONData, by = "Date")
SARONData$SARON <- na.approx(SARONData$Value)

SOFRurl <- "https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1319&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=SOFR&scale=left&cosd=2018-04-03&coed=2024-11-09&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-11-12&revision_date=2024-11-12&nd=2018-04-03"
SOFRData <- fetch_data(SOFRurl, skip_rows = 1, date_format = "%Y-%m-%d")
date_range <- seq(from = min(SOFRData$Date), to = max(SOFRData$Date), by = "day")
SOFRData <- data.frame(Date = date_range) %>%
  left_join(SOFRData, by = "Date")
SOFRData$SOFR <- na.approx(SOFRData$Value)

ERurl <- "https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2024&filetype=csv&label=include&layout=seriescolumn"
ERData <- fetch_data(ERurl, skip_rows = 6, date_format = "%Y-%m-%d")
date_range <- seq(from = min(ERData$Date), to = max(ERData$Date), by = "day")
ERData <- data.frame(Date = date_range) %>%
  left_join(ERData, by = "Date")
ERData$Value <- na.approx(ERData$Value)
ERData <- ERData %>%
  mutate(USDCHF = na.approx(as.numeric(Value)),
         CHFUSD = 1 / USDCHF,
         LogCHFUSD = log(CHFUSD),
         LaggedLogCHFUSD = lag(LogCHFUSD),
         LogDifferenceCHFUSD = LogCHFUSD - LaggedLogCHFUSD)

SARONData <- SARONData %>% select(Date, SARON)
SOFRData <- SOFRData %>% select(Date, SOFR)
ERData <- ERData %>% select(Date, LogDifferenceCHFUSD)
combinedData <- reduce(list(SARONData, SOFRData, ERData), full_join, by = "Date")

first_valid_date <- combinedData %>% 
  filter(!is.na(SARON) & !is.na(SOFR) & !is.na(LogDifferenceCHFUSD)) %>%
  summarize(first_date = min(Date)) %>%
  pull(first_date)

last_valid_date <- combinedData %>% 
  filter(!is.na(SARON) & !is.na(SOFR) & !is.na(LogDifferenceCHFUSD)) %>%
  summarize(last_date = max(Date)) %>%
  pull(last_date)

trimmedData <- combinedData %>%
  filter(Date >= first_valid_date & Date <= last_valid_date)

date_range <- seq(from = min(trimmedData$Date), to = max(trimmedData$Date), by = "day")
missing_dates <- setdiff(date_range, trimmedData$Date)

if (length(missing_dates) == 0) {
  print("No missing dates in the series.")
} else {
  print(paste("Missing dates:", missing_dates))
}

write_csv(trimmedData, "cleaned_dataR.csv")
