setwd("/Users/kyritsis/Desktop/Financial Big Data/final")

library(xts)
library(PerformanceAnalytics)
library(ggplot2)

data = readRDS(file = 'Data/US_stocks_filtered.rds')
data = data["2000/",]

log_returns = Return.calculate(data, method = 'log')
log_returns = log_returns[-c(1),]

alldates=as.character(index(log_returns))

# Take only the dates after 2007-12-18
alldates = alldates[2001:4249]
Tin=2000

df_mean_variance = data.frame(matrix(ncol = 4, nrow = length(alldates)))
colnames(df_mean_variance) = c('pmPD', 'psPD', 'pmLW', 'psLW')
rownames(df_mean_variance) = alldates

setwd("/Users/kyritsis/Desktop/Financial Big Data/final/Data/portfolio_attributes_mean_variance/")

## In sample Expected returns and standard deviations

for (day in alldates) {
  fname = paste0("port_PD_mv_", day,".rds")
  temp = readRDS(fname)
  df_mean_variance[day,1] = temp$pm
  df_mean_variance[day,2] = temp$ps
  print(day)
}

for (day in alldates) {
  fname = paste0("port_LW_mv_", day,".rds")
  temp = readRDS(fname)
  df_mean_variance[day,3] = temp$pm
  df_mean_variance[day,4] = temp$ps
  print(day)
}

expected_return_std = as.xts(df_mean_variance)
setwd("/Users/kyritsis/Desktop/Financial Big Data/final")


png("figures/fig_mean_variance_ins_r.png", width = 5, height = 4, units = 'in', res = 300)
ggplot(expected_return_std, aes(index(expected_return_std))) + 
  geom_line(aes(y = expected_return_std[,1], colour = "Original Covariance")) + 
  geom_line(aes(y = expected_return_std[,3], colour = "Ledoit-Wolf")) +
  labs(x="Year",y="In-sample expected return") +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
dev.off()

png("figures/fig_mean_variance_ins_std.png", width = 5, height = 4, units = 'in', res = 300)
ggplot(expected_return_std, aes(index(expected_return_std))) + 
  geom_line(aes(y = expected_return_std[,2], colour = "Original Covariance")) +
  geom_line(aes(y = expected_return_std[,4], colour = "Ledoit-Wolf")) +
  labs(x="Year",y="In-sample expected standard deviation") + 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
dev.off()

## Out of sample Expected returns and standard deviations

setwd("/Users/kyritsis/Desktop/Financial Big Data/final/Data/portfolio_attributes_mean_variance/")

df_out_of_sample_mean_variance = data.frame(matrix(ncol = 4, nrow = length(alldates[1:2049])))
colnames(df_out_of_sample_mean_variance) = c('pmPD', 'psPD', 'pmLW', 'psLW')
rownames(df_out_of_sample_mean_variance) = alldates[1:2049]

index = 1
for (day in alldates[1:2049]) {
  fname = paste0("port_PD_mv_", day,".rds")
  temp = readRDS(fname)
  weights = temp$pw
  dates = paste0(alldates[index+1],'::',alldates[index+200])
  myreturns = log_returns[dates]
  
  yy = myreturns %*% weights
  pm = mean(yy)
  ps = sd(yy)
  df_out_of_sample_mean_variance[day,1] = pm
  df_out_of_sample_mean_variance[day,2] = ps
  index = index + 1
  print(day)
}

index = 1
for (day in alldates[1:2049]) {
  fname = paste0("port_LW_mv_", day,".rds")
  temp = readRDS(fname)
  weights = temp$pw
  dates = paste0(alldates[index+1],'::',alldates[index+200])
  myreturns = log_returns[dates]
  
  yy = myreturns %*% weights
  pm = mean(yy)
  ps = sd(yy)
  df_out_of_sample_mean_variance[day,3] = pm
  df_out_of_sample_mean_variance[day,4] = ps
  index = index + 1
  print(day)
}

out_of_sample_expected_return_std = as.xts(df_out_of_sample_mean_variance)
setwd("/Users/kyritsis/Desktop/Financial Big Data/final")

png("figures/fig_mean_variance_ous_r.png", width = 5, height = 4, units = 'in', res = 300)
ggplot(out_of_sample_expected_return_std, aes(index(out_of_sample_expected_return_std))) + 
  geom_line(aes(y = out_of_sample_expected_return_std[,1], colour = "Original Covariance")) + 
  geom_line(aes(y = out_of_sample_expected_return_std[,3], colour = "Ledoit-Wolf")) +
  labs(x="Year",y="Out-of-sample expected return") + 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
dev.off()

png("figures/fig_mean_variance_ous_std.png", width = 5, height = 4, units = 'in', res = 300)
ggplot(out_of_sample_expected_return_std, aes(index(out_of_sample_expected_return_std))) + 
  geom_line(aes(y = out_of_sample_expected_return_std[,2], colour = "Original Covariance")) +
  geom_line(aes(y = out_of_sample_expected_return_std[,4], colour = "Ledoit-Wolf")) +
  labs(x="Year",y="Out-of-sample expected standard deviation") + 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
dev.off()

# Calculate the cumulative sum
cum_sum_PD = cumsum(out_of_sample_expected_return_std[,1])
cum_sum_LW = cumsum(out_of_sample_expected_return_std[,3])

cum_sum = merge(cum_sum_PD, cum_sum_LW)

png("figures/fig_mean_variance_cum.png", width = 5, height = 4, units = 'in', res = 300)
ggplot(cum_sum, aes(index(cum_sum))) + 
  geom_line(aes(y = cum_sum[,1], colour = "Original Covariance")) +
  geom_line(aes(y = cum_sum[,2], colour = "Ledoit-Wolf")) +
  labs(x="Year",y="Out of sample cumulative return") + 
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())
dev.off()
