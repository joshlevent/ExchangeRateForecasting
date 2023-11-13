# ------------------------------------------------------------------------------
# Exchange Rate Forecasting
# ------------------------------------------------------------------------------
#   Applied Macroeconometrics, University of Neuchatel
#   Josh Levent, Julien Beaud, 2023
# ------------------------------------------------------------------------------

# ------------------------------------------------------------------------------
# Packages and functions that we need
# ------------------------------------------------------------------------------
library(tsbox)
library(forecast)
library(xts)
library(tsbox)
library(ggplot2)   
library(reshape2)
library(dataseries)
library(seasonal)
library(readxl) 
library(XML)
library(CADFtest)
library(quantmod)
library(gridExtra)
library(forecast)


# Delete all objects in the memory
rm(list=ls())

# Load user-defined commands and packages
source("UserPackages.R")

# Create results folder
mainDir <- getwd()
outDir <- makeOutDir(mainDir, "/ResultsApp6")


# ------------------------------------------------------------------------------
# Import and clean Data
# ------------------------------------------------------------------------------

# SOFR

Data <- read.csv("https://fred.stlouisfed.org/graph/fredgraph.csv?bgcolor=%23e1e9f0&chart_type=line&drp=0&fo=open%20sans&graph_bgcolor=%23ffffff&height=450&mode=fred&recession_bars=on&txtcolor=%23444444&ts=12&tts=12&width=1319&nt=0&thu=0&trc=0&show_legend=yes&show_axis_titles=yes&show_tooltip=yes&id=SOFR&scale=left&cosd=2018-04-03&coed=2024-11-09&line_color=%234572a7&link_values=false&line_style=solid&mark_type=none&mw=3&lw=2&ost=-99999&oet=99999&mma=0&fml=a&fq=Daily&fam=avg&fgst=lin&fgsnd=2020-02-01&line_index=1&transformation=lin&vintage_date=2024-11-12&revision_date=2024-11-12&nd=2018-04-03")

SOFR   = xts(Data[,2], order.by = as.Date(paste(Data[,1],"-01", sep = "")))

# Swiss Dollar exchange rate

Data <- read.csv("https://www.federalreserve.gov/datadownload/Output.aspx?rel=H10&series=f838388dca2fd4e8bdfb846f3d2c35df&lastobs=&from=01/01/1971&to=10/09/2024&filetype=csv&label=include&layout=seriescolumn", skip = 5)

SWISSDOLLAR  = xts(Data[,2], order.by = as.Date(paste(Data[,1],"-01", sep = "")))

# Saron, The semicolon is specified as the separator, skip = 3 is used to skip the first three rows which are headers

file_url <- "https://www.six-group.com/exchanges/downloads/indexdata/hsrron.csv"

Data <- read.csv(file_url, sep = ";", skip = 3, header = TRUE)

Data[,1] <- as.Date(Data[,1], format = "%d.%m.%Y")

Saron  = xts(Data[,2], order.by = as.Date(paste(Data[,1],"-01", sep = "")))

# Swiss & US Yield Bond

# Plot

plot(SOFR)

