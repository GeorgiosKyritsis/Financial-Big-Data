setwd("~/Desktop/Financial Big Data/final")

library(tawny)
library(quadprog)
library(xts)
library(PerformanceAnalytics)

data = readRDS(file = 'Data/US_stocks_filtered.rds')
data = data["2000/",]

log_returns = Return.calculate(data, method = 'log')
log_returns = log_returns[-c(1),]

alldates=as.character(index(log_returns))

Tin=2000

dirSavePortAttr = "Data/portfolio_attributes_min_variance/"
dir.create(dirSavePortAttr,recursive = TRUE)


for(mydate in as.character(tail(alldates,-Tin))){
  firstDate=alldates[which(alldates %in% mydate)-Tin+1]
  filenamePD=paste0(dirSavePortAttr,"/port_PD","_",mydate,".rds")
  filenameLW=paste0(dirSavePortAttr,"/port_LW","_",mydate,".rds")
  
  print(paste(firstDate,mydate))
  myreturns=log_returns[paste0(firstDate,"::",mydate)]
  
  # load the covariance matrix
  file_PD = paste0("Data/covariances/","cov_us_stocks_PD", mydate, ".rds")
  print(file_PD)
  file_LW = paste0("Data/covariances/","cov_us_stocks_LW", mydate, ".rds")
  print(file_LW)
  
  PD = readRDS(file = file_PD)
  LW = readRDS(file = file_LW)
  
  N = ncol(LW)
  
  # Solve the optimization problem
  zeros = array(0, dim = c(N,1))
  
  aMat = cbind(t(array(1, dim = c(1,N))), diag(N))
  b0 = as.matrix(c(1, rep.int(0,N)))
  res_PD = solve.QP(PD, zeros, aMat, bvec=b0, meq = 1)
  res_LW = solve.QP(LW, zeros, aMat, bvec=b0, meq = 1)
  
  # calculate the weights, expected return, standard deviation
  y_PD <- myreturns %*% res_PD$solution
  port_PD <- list(pw = round(res_PD$solution,3), px = y_PD, pm = mean(y_PD), ps = sd(y_PD))
  
  # calculate the weights, expected return, standard deviation
  y_LW <- myreturns %*% res_LW$solution
  port_LW <- list(pw = round(res_LW$solution,3), px = y_LW, pm = mean(y_LW), ps = sd(y_LW))
  
  # save 
  saveRDS(port_PD,file=filenamePD)
  saveRDS(port_LW,file=filenameLW)
}
