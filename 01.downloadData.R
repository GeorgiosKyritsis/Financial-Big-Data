setwd("/Users/kyritsis/Desktop/Financial Big Data/final")
library(tseries)
library(xts)
require(TTR)

# Download Symbols NYSE, NASDAQ, AMEX
SYMs = TTR::stockSymbols()

tickerSymbols = c("^GSPC", "^IXIC", SYMs$Symbol)

# Remove tickers from original dataset
# I couldn't download data for these ticker symbols
tickersToBeRemoved = readRDS(file = "Data/TickersToBeRemoved.rds")

tickerSymbols = tickerSymbols [!tickerSymbols %in% tickersToBeRemoved]

data = list()

# Download historical finance data from 01.01.1950
for (i in 1:length(tickerSymbols)) {
  print(i)
  print(tickerSymbols[i])
  data[[i]] = get.hist.quote(instrument=tickerSymbols[i],start=as.Date("1950-01-01"),
                               end=Sys.Date(),quote="AdjClose",quiet=T)
  
}

data = do.call("merge", data)

# Convert to xts object
US_stocks = as.xts(data)

names(US_stocks) = tickerSymbols

# Save file
saveRDS(US_stocks, file = 'Data/US_stocks.rds')
