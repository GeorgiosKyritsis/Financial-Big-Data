setwd("~/Desktop/Financial Big Data/final")

library(caret)
library(dplyr)         # Used by caret
library(xts)
library(quantmod)
library(TTR)
library(ggplot2)

data = readRDS("Data/US_stocks_filtered.rds")

# SP500 prices
sp = data[,1]

# APPLE prices
aapl = data[,"AAPL"]

# Compute rolling annual log returns (approximately 252 days per year)
# Choose which to comment to get results for SP500 or APPLE
#ret = ROC(sp, n = 252, type = 'continuous')
ret = ROC(aapl, n = 252, type = 'continuous')

# Model Inputs:
# Define matrix of features (each column is a feature)
# Features: lags 1,2,3,5
feat = merge(na.trim(lag(ret,1)),na.trim(lag(ret,2)),
                na.trim(lag(ret,3)),na.trim(lag(ret,5)),
                all=FALSE)

# add TARGET. We want to predict RETURN
dataset = merge(feat,ret,all=FALSE)

colnames(dataset) = c("lag.1", "lag.2", "lag.3","lag.5", "TARGET")

index = 1:nrow(dataset)
trainIndex <- createDataPartition(index,p=.5,list=FALSE)
trainData <- as.data.frame(dataset[trainIndex,])
testData  <- as.data.frame(dataset[-trainIndex,])

## SUPPORT VECTOR MACHINE MODEL
set.seed(123)
# Setup for cross validation (10 fold CV)
ctrl <- trainControl(method="repeatedcv", repeats=5)

#Train and Tune the SVM
tune <- train(x=trainData[,-ncol(trainData)],
                y= trainData[,ncol(trainData)],
                  method = "svmLinear",   # Linear kernel
                  tuneLength = 9,					# 9 values of the cost function
                  preProc = c("center","scale"),  # Center and scale data
                  metric="RMSE",
                  trControl=ctrl)

tune

# Predict returns
pred = predict(tune, testData[,-ncol(testData)])

#the true series to predict
actualTS=testData[,ncol(testData)] 
predictTS=pred

# Evaluation of return prediction
rmse = sqrt(mean((predictTS - actualTS)^2)) 
print(rmse)

png("figures/figSVMAPPLE.png", width = 5, height = 4, units = 'in', res = 300)

q2 = qplot(actualTS, predictTS) + 
  xlab("Actual Time series") + ylab("Predicted Time series")
q2
dev.off()
