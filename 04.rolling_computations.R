setwd("~/Desktop/Financial Big Data/final")

library(xts)
library(PerformanceAnalytics)
library(WGCNA)
library(tawny)
library(Matrix)

data = readRDS(file = 'Data/US_stocks_filtered.rds')

data = data["2000/",]

log_returns = Return.calculate(data, method = 'log')
log_returns = log_returns[-c(1),]

alldates=as.character(index(log_returns))

Tin=2000

# Create an empty folder to store all the covariance matrices
dirSaveCov = "Data/covariances/"
dir.create(dirSaveCov,recursive = TRUE)

for(mydate in as.character(tail(alldates,-Tin))){
  firstDate=alldates[which(alldates %in% mydate)-Tin+1]
  filenameCov=paste0(dirSaveCov,"/cov_us_stocks","_",mydate,".rds")
  filenameCov_positive_definite=paste0(dirSaveCov,"/cov_us_stocks","_PD",mydate,".rds")
  filenameCov_Ledoit_Wolf=paste0(dirSaveCov,"/cov_us_stocks","_LW",mydate,".rds")
  
  print(paste(firstDate,mydate))
  myreturns=log_returns[paste0(firstDate,"::",mydate)]

  # Compute correlation matrix
  Corr=cor(myreturns)
  standard_deviations = apply(myreturns, 2, sd)
  b = standard_deviations %*% t(standard_deviations)
  
  # Compute covariance matrix
  Cov = b * Corr
  colnames(Cov)=colnames(myreturns)
  rownames(Cov)=colnames(myreturns)
  
  Cov_positive_definite = nearPD(Cov)
  Cov_positive_definite = as.matrix(Cov_positive_definite$mat)
  
  # Covariance shrinkage (Ledoit - Wolf) It is always positive definite!!!
  Cov_LW = cov_shrink(myreturns)
  colnames(Cov_LW)=colnames(myreturns)
  rownames(Cov_LW)=colnames(myreturns)
  
  # save covariance matrix
  saveRDS(Cov,file=filenameCov)
  saveRDS(Cov_positive_definite,file=filenameCov_positive_definite)
  saveRDS(Cov_LW,file=filenameCov_Ledoit_Wolf)
}
