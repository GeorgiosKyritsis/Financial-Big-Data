setwd("/Users/kyritsis/Desktop/Financial Big Data/final")

library(xts)
library(ggplot2)

data = readRDS(file = 'Data/US_stocks.rds')

# Some numbers!
prod(dim(data))       # 104194944 elements
sum(is.na(data)) / prod(dim(data)) * 100      # 79.05625% NAs
dim(data)

# Count the number of NAs values
num_NAs=apply(data,1,function(myline) {sum(is.na(myline))})
num_NAs = as.xts(num_NAs)

num_NAs.df = data.frame(date=index(num_NAs), y = coredata(num_NAs))

png("figures/fig0.png", width = 5, height = 4, units = 'in', res = 300)

ggplot( num_NAs.df, aes(date)) + 
  geom_line(aes(y = y)) +
  labs(x = "dates", y = "Number of missing values")

dev.off()

# Many NAs in every row before 1990
# so I take only prices after 1990
data = data['1990::']

# Some numbers!
dim(data)  # 6936 x 6132
prod(dim(data))       # 42531552 elements
sum(is.na(data)) / prod(dim(data)) * 100      # 51.48476% NAs

# 1st column is the S&P500 index
# Remove the lines where S&P500 is NA
data = data[!is.na(data[,1]),]
dim(data)

#removes lines which are entirely made up of NAs if any
sel_all_NA = apply(data,1,function(x) all(is.na(x)))
data = data[!sel_all_NA]

# Count the number of NAs values per Ticker Symbol
num_of_NAs=apply(data,2,function(myline) {sum(is.na(myline))})

# Count ticker symbols with zero NAs
count_tickers_with_0_NA = sum(num_of_NAs == 0)  # 689 tickers only, We want more tickers

# Let's count the NAs per line
num_of_NAs_per_line=apply(data,1,function(myline) {sum(is.na(myline))})
num_of_NAs_per_line = as.xts(num_of_NAs_per_line)  # decreasing with the date increasing

data = data['1991::']
dim(data)  # 6525 x 6132

# Count the number of NAs values per Ticker Symbol
num_of_NAs=apply(data,2,function(myline) {sum(is.na(myline))})

# Count ticker symbols with zero NAs
count_tickers_with_0_NA = sum(num_of_NAs == 0)  # 928 tickers!

data = data['1992::']
dim(data)  # 6272 x 6132

# Count the number of NAs values per Ticker Symbol
num_of_NAs=apply(data,2,function(myline) {sum(is.na(myline))})

# Count ticker symbols with zero NAs
count_tickers_with_0_NA = sum(num_of_NAs == 0)  # 1018 tickers!!!, I stop here

# Get Ticker symbols with no NAs
sel_tickers = (num_of_NAs == 0)

# Get the names of tickers with no NAs
columns = c();
for (i in 1:length(sel_tickers)) {
  if (sel_tickers[i] == TRUE)
    columns = c(columns, names(sel_tickers[i]))
}

data = data[,  colnames(data) %in% columns]

# Some numbers!
prod(dim(data))       # 6384896 elements
sum(is.na(data)) / prod(dim(data)) * 100      # 0% NAs
dim(data)  # 6272 x 1018

saveRDS(data,file='Data/US_stocks_filtered.rds')
