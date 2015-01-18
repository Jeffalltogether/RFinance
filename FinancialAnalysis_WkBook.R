### Financial Analysis Test
### Hypothesis: Can we predict the next week's performance of a stock based on previous performance information?


### libraries and working directory
require(quantmod)
require(ISLR)
require(TTR)
require(plyr)
require(class)
require(caret)
require(kernlab)
require(leaps)

setwd("C:/Users/jeffthatcher/Cloud Drive/Documents/R/StockAnalysis_01132015")
options("getSymbols.warning4.0"=FALSE)

### Use single quotes and specify data source:
myStock = "GOOGL"       # <<-------------------------------- enter stock name here
SP500 = "SPY"
JunkB = "VT SMX"

### Download Stock Data
tryCatch(StockXTS <- OHLCV(getSymbols(myStock, auto.assign=FALSE, src = "google")))  # src = "yahoo" is the default
tryCatch(SP500XTS <- OHLCV(getSymbols(SP500, auto.assign=FALSE, src = "google")))  # src = "yahoo" is the default
tryCatch(JunkBXTS <- OHLCV(getSymbols(JunkB, auto.assign=FALSE, src = "google")))  # src = "yahoo" is the default

MergedXTS = merge(StockXTS, SP500XTS, JunkBXTS)

### Plot Stock Data
candleChart(StockXTS, subset='201410:/', name = myStock, theme='white',TA=NULL) #yyyymmdd::yyyymmdd (start time :: end time)
addVo()
addRSI()
addBBands()
addMACD(32,50,12)

### Re-name an xts and replace all the headers
stockColNames1 <- sub(myStock, "stockDataXTS", names(StockXTS))
stockColNames2 <- sub(SP500, "stockDataXTS", names(SP500XTS))
stockColNames3 <- sub(JunkB, "stockDataXTS", names(JunkBXTS))
stockColNames <- c(stockColNames1, stockColNames2, stockColNames3)

names(MergedXTS) = stockColNames # 13 characters shorter. 
MergedXTS=na.omit(MergedXTS)

StockXTS = MergedXTS[,1:5]
SP500XTS = MergedXTS[,6:10]
JunkBXTS = MergedXTS[,11:15]

rm(MergedXTS)
### Convert raw stock data into a vector data frame
source("myStockVectors.R")
Stockdata <- myStockVectors(StockXTS)
SP500data <- myStockVectors(SP500XTS)
JunkBdata <- myStockVectors(JunkBXTS)

### Calculate the Indicators for the stock
source("myIndicators.R")
StockInd <- myIndicators(Stockdata, StockXTS)
SP500Ind <- myIndicators(SP500data, SP500XTS)
JunkBInd <- myIndicators(JunkBdata, JunkBXTS)

rm(StockXTS, SP500XTS, JunkBXTS)
### -------------------Machine Learning-------------------

# Goal is to predict the 1-day, 5-day, ... n-day forcast for a stock 
# positive movement from t0 to tn using X (starting with 10) days of previous indicators as data
# there can be three Classes. A is > 2% return; B is > 0 to 2 % return; C is -2 to 0 % return; D is <-2 % return

### Create Classes to Predict
# calcualte the percent [%] five (5) day return
source("weeklyReturn.R")
StockClasses <- weeklyReturn(Stockdata)
SP500Classes <- weeklyReturn(SP500data)
JunkBClasses <- weeklyReturn(JunkBdata)

### Generate Features for each Class
# examples:
# class, preceeding 10 days close average centered at day10, preceeding 10 days average rsi, 
# preceeding 5 days average rsi, preceeding day rsi, preceeding 10 day over sold average, 
# preceeding 5 day over sold average, preceeding 10 day undersold average, preceeding 5 day undersold average

# preceeding daily, weekly, monthly, quarterly, yearly returns
source("myPrecedingReturns.R")
StockPrevRets <- myPrecedingReturns(StockInd)
SP500PrevRets <- myPrecedingReturns(SP500Ind)
JunkBPrevRets <- myPrecedingReturns(JunkBInd)

# preceeding daily, weekly, monthly, quarterly, yearly RSI
source("myPrecedingRSI.R")
StockPrevRSIs <- myPrecedingRSI(StockInd)
SP500PrevRSIs <- myPrecedingRSI(SP500Ind)
JunkBPrevRSIs <- myPrecedingRSI(JunkBInd)

# preceeding 5, 10, 15, 20, yearly SMA (simple moving average)
source("myPrecedingSMA.R")
StockPrevSMAs <- myPrecedingSMA(StockInd)
SP500PrevSMAs <- myPrecedingSMA(SP500Ind)
JunkBPrevSMAs <- myPrecedingSMA(JunkBInd)

# preceeding 10 days average Oversold
source("WklyAveOverUnderSold.R")
StockOverUnder <- WklyAveOverUnderSold(StockInd)
SP500OverUnder <- WklyAveOverUnderSold(SP500Ind)
JunkBOverUnder <- WklyAveOverUnderSold(JunkBInd)

### Ground Truth Dataframe
source("mergeStockFeatures.R")
source("mergeSP500Features.R")
source("mergeJunkBFeatures.R")
StockFeaturesDF <- mergeStockFeatures(StockClasses, StockOverUnder, StockPrevRSIs, StockPrevRets, StockPrevSMAs)
SP500FeaturesDF <- mergeSP500Features(SP500Classes, SP500OverUnder, SP500PrevRSIs, SP500PrevRets, SP500PrevSMAs)
JunkBFeaturesDF <- mergeJunkBFeatures(JunkBClasses, JunkBOverUnder, JunkBPrevRSIs, JunkBPrevRets, JunkBPrevSMAs)

rm(StockClasses, StockOverUnder, StockPrevRSIs, StockPrevRets, StockPrevSMAs, 
   SP500Classes, SP500OverUnder, SP500PrevRSIs, SP500PrevRets, SP500PrevSMAs, 
   JunkBClasses, JunkBOverUnder, JunkBPrevRSIs, JunkBPrevRets, JunkBPrevSMAs)
# remove columns that we do not want to include in the analysis
drops <- c("prev2DaysRSI","prev4DaysRSI", "prev8DaysRSI", "prev15DaysRSI", "prev20DaysRSI", "prev25DaysRSI", "prev2DaysRet","prev4DaysRet", "prev8DaysRet", "prev15DaysRet", "prev20DaysRet", "prev25DaysRet", "prev2DaysSMA","prev4DaysSMA", "prev8DaysSMA", "prev15DaysSMA", "prev20DaysSMA", "prev25DaysSMA")
TruncStockFeaturesDF <- StockFeaturesDF[,!(names(StockFeaturesDF) %in% drops)]
TruncSP500FeaturesDF <- StockFeaturesDF[,!(names(SP500FeaturesDF) %in% drops)]
TruncJunkBFeaturesDF <- StockFeaturesDF[,!(names(JunkBFeaturesDF) %in% drops)]

# merge the three stock ticker data frames
# I rean out of memory when first attempting this and made this change to the Target in windows "C:\Program Files\RStudio\bin\rstudio.exe" --max-mem-size=2GB
dat <- merge(StockFeaturesDF, SP500FeaturesDF, by = StockFeaturesDF$row.names, suffixes = c("A","B"))
AllFeaturesDF <- merge(dat, JunkBFeaturesDF, by = dat$row.names, suffixes = c("C","D"))


### Generate Training and Test Data
# inTrain <- createDataPartition(y = stockFeaturesDF$weeklyClass, p = 0.60, list = FALSE)
# training = stockFeaturesDF[inTrain,]
# test = stockFeaturesDF[-inTrain,]
# classTesting = as.vector(test$weeklyClass)
# names(training)

### Fit Logistic Regression Model to Weekly Returns Price of One Stock
regfit.full=regsubsets(weeklyClass.Stock~.,AllFeaturesDF[,1:50],nvmax=49, really.big = TRUE)
reg.summary=summary(regfit.full)

### Calculate the prediction error for regfit
### Cross-Validation
AllFeaturesDF=na.omit(AllFeaturesDF) # eliminate the missing values from the dataframe
source("predict.R") # find the predict.regsubsets function we created for regsubsets
set.seed(777)
folds=sample(rep(1:10,length=nrow(AllFeaturesDF)))
testing.errors=matrix(NA,10,35) #matrix for our errors, 10 -rows for each of the 10-folds, 8 subsets for each of our variables (36 - weeklyClass = 19)
training.errors=matrix(NA,10,35)
for(k in 1:10){
  best.fit=regsubsets(weeklyClass.Stock~.,data=AllFeaturesDF[folds!=k,],nvmax=35,method
                      ="forward", really.big = FALSE)
  for(i in 1:35){
    pred.test=predict(best.fit,AllFeaturesDF[folds==k,], id=i)
    predtest=round(pred.test,digits=0)
    pred.train=predict(best.fit,AllFeaturesDF[folds!=k,], id=i)
    predtrain=round(pred.train,digits=0)
    testing.errors[k,i]=mean( (AllFeaturesDF$weeklyClass.Stock[folds==k]-predtest)^2)
    training.errors[k,i]=mean( (AllFeaturesDF$weeklyClass.Stock[folds!=k]-predtrain)^2)
  }
}
plot(regfit.full, scale = "r2")
rmse.testing=sqrt(apply(testing.errors,2,mean))
rmse.training=sqrt(apply(training.errors,2,mean))
plot(rmse.training, pch=19, type="b", col="blue",
     xlab="Number of Variables",
     ylab="Root Mean Square Error",
     main="Cross-Validation",
     sub ="Blue - Training Accuracy, Black - Test Accuracy",
     ylim=c(0.5,0.9))
pmin = which.min(rmse.training)  # Find the minimum point
points(pmin, rmse.training[pmin], col ="red",cex =2, pch =20)  #plot the 
#par(new=TRUE)
lines(rmse.testing,pch=19,type="b",col="black",
      ylim=c(0.5,0.9))
pmin = which.min(rmse.testing)  # Find the minimum point
points(pmin, rmse.testing[pmin], col ="red",cex =2, pch =20)  #plot the 

