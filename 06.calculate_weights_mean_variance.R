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

dirSavePortAttr = "Data/portfolio_attributes_mean_variance/"
dir.create(dirSavePortAttr,recursive = TRUE)


for(mydate in as.character(tail(alldates,-Tin))){
  firstDate=alldates[which(alldates %in% mydate)-Tin+1]
  filenamePD=paste0(dirSavePortAttr,"/port_PD_mv","_",mydate,".rds")
  filenameLW=paste0(dirSavePortAttr,"/port_LW_mv","_",mydate,".rds")
  
  print(paste(firstDate,mydate))
  myreturns=log_returns[paste0(firstDate,"::",mydate)]
  
  # load the covariance matrix
  file_PD = paste0("Data/covariances/","cov_us_stocks_PD", mydate, ".rds")
  print(file_PD)
  file_LW = paste0("Data/covariances/","cov_us_stocks_LW", mydate, ".rds")
  print(file_LW)
  
  PD = readRDS(file = file_PD)
  LW = readRDS(file = file_LW)
  
  # Solve the optimization problem
  ret_matrix = data.matrix(as.data.frame(myreturns))
  averet = matrix(colMeans(ret_matrix),nrow=1)
  
  dvec = t(averet)
  A.Equality = matrix(rep(1,1018), ncol=1)
  Amat = cbind(A.Equality, dvec, diag(1018), -diag(1018))
  bvec = c(1, 0.0005, rep(0, 1018), rep(-0.05, 1018))
  qp_PD = solve.QP(PD, dvec, Amat, bvec, meq=1)
  qp_LW = solve.QP(LW, dvec, Amat, bvec, meq=1)
  
  # calculate the weights, expected return, standard deviation
  y_PD <- myreturns %*% qp_PD$solution
  port_PD <- list(pw = round(qp_PD$solution,3), px = y_PD, pm = mean(y_PD), ps = sd(y_PD))
  
  # calculate the weights, expected return, standard deviation
  y_LW <- myreturns %*% qp_LW$solution
  port_LW <- list(pw = round(qp_LW$solution,3), px = y_LW, pm = mean(y_LW), ps = sd(y_LW))
  
  # save 
  saveRDS(port_PD,file=filenamePD)
  saveRDS(port_LW,file=filenameLW)
}
