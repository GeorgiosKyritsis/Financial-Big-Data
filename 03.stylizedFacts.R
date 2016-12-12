setwd("~/Desktop/Financial Big Data/final")

library(xts)
library(PerformanceAnalytics)
library(poweRlaw)
library(ggplot2)
library(quantmod)
library(normtest)

data = readRDS(file = 'Data/US_stocks_filtered.rds')

AAPL = data$AAPL
AAPLReturns = Return.calculate(AAPL, method = "log")
AAPLReturns = AAPLReturns[-c(1),]
kurtosis(AAPLReturns)

bacf <- acf(AAPLReturns, plot = FALSE)
bacfdf <- with(bacf, data.frame(lag, acf))
bacfdf$lag = 1:nrow(bacfdf)
conf.level <- 0.95
ciline <- qnorm((1 - conf.level)/2)/sqrt(length(AAPLReturns))

png("figures/fig1.png", width = 5, height = 4, units = 'in', res = 300)

q = ggplot(data = bacfdf, mapping = aes(x = lag, y = acf)) +
  geom_hline(aes(yintercept = 0)) +
  geom_segment(mapping = aes(xend = lag, yend = 0))

q = q + geom_hline(yintercept = -ciline, color = "blue",
                    size = 0.2)
q = q + geom_hline(yintercept = ciline, color = "blue",
                    size = 0.2)
q = q + geom_hline(yintercept = 0, color = "red", size = 0.3)

q = q + labs(x="Lag (number of ticks)",y="Sample autocorrelation") 
q
dev.off()


## Power Law decay of the returns of the return distribution

rAAPLle = coredata(AAPLReturns)
rAAPLle = rAAPLle[,1]

# Take the absolute values
rAAPLle = abs(rAAPLle)

# Remove zero returns
rAAPLle = rAAPLle[rAAPLle !=0]

# Continuous power law
m_rAAPLle = conpl$new(rAAPLle)
est = estimate_xmin(m_rAAPLle)
est$pars
m_rAAPLle$setXmin(est)
dd = plot(m_rAAPLle)
line = lines(m_rAAPLle, col=3, lwd=2)

png("figures/fig2.png", width = 5, height = 4, units = 'in', res = 300)

q2 = qplot(dd$x, dd$y, log="xy") + geom_line(aes(x = line$x, y = line$y), colour = "red") + 
xlab("|r|") + ylab("1 - P(|return| < |r|)")
q2
dev.off()


# Volatility Clustering
SP500 = readRDS(file = 'Data/US_stocks.rds')
SP500 = SP500[,1]

# remove NAs
SP500 = na.omit(SP500)

# Calculate log returns
SP500Returns = Return.calculate(SP500, method = "log")
SP500Returns = SP500Returns[-c(1),]

dates = index(SP500Returns)
dat = coredata(SP500Returns)
returns.df = data.frame(dates, dat)

png("figures/fig3.png", width = 5, height = 4, units = 'in', res = 300)
q3 = ggplot(returns.df, aes(dates, dat)) + geom_line() + xlab("dates") + ylab("log returns")
q3
dev.off()

kurtosis(SP500Returns)
skewness(SP500Returns)

## Agregational Gaussianity
dailyReturnsSP500 = dailyReturn(SP500, type = 'log')
weeklyReturnsSP500 = weeklyReturn(SP500, type = 'log')
monthlyReturnsSP500 = monthlyReturn(SP500, type = 'log')
quarterlyReturnsSP500 = quarterlyReturn(SP500, type = 'log')
annualReturnsSP500 = annualReturn(SP500, type = 'log')

# Summary statistics for log-returns
mean(dailyReturnsSP500)
mean(weeklyReturnsSP500)
mean(monthlyReturnsSP500)
mean(quarterlyReturnsSP500)
mean(annualReturnsSP500)

median(dailyReturnsSP500)
median(weeklyReturnsSP500)
median(monthlyReturnsSP500)
median(quarterlyReturnsSP500)
median(annualReturnsSP500)

sd(dailyReturnsSP500)
sd(weeklyReturnsSP500)
sd(monthlyReturnsSP500)
sd(quarterlyReturnsSP500)
sd(annualReturnsSP500)

skewness(dailyReturnsSP500)
skewness(weeklyReturnsSP500)
skewness(monthlyReturnsSP500)
skewness(quarterlyReturnsSP500)
skewness(annualReturnsSP500)

kurtosis(dailyReturnsSP500)
kurtosis(weeklyReturnsSP500)
kurtosis(monthlyReturnsSP500)
kurtosis(quarterlyReturnsSP500)
kurtosis(annualReturnsSP500)

#Jarque-Bera test
ajb.norm.test(dailyReturnsSP500, nrepl=2000)
ajb.norm.test(weeklyReturnsSP500, nrepl=2000)
ajb.norm.test(monthlyReturnsSP500, nrepl=2000)
ajb.norm.test(quarterlyReturnsSP500, nrepl=2000)
ajb.norm.test(annualReturnsSP500, nrepl=2000)

# ACF of absolute returns
bacf_SP <- acf(abs(SP500Returns), plot = FALSE, lag.max = 500)
bacfdf_SP <- with(bacf_SP, data.frame(lag, acf))
bacfdf_SP$lag = 1:nrow(bacfdf_SP)

png("figures/fig4.png", width = 5, height = 4, units = 'in', res = 300)

q2 = qplot(bacfdf_SP$lag, bacfdf_SP$acf, log="xy") + 
  xlab("t (day)") + ylab("Autocorrelation |r|")
q2
dev.off()
