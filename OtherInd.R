### S&P 500 indicators
OtherInd <- function(stockDataXTS){

  
  ### Convert raw stock data into a vector data frame
  source("myStockVectors.R")
  500Data <- myStockVectors(stockDataXTS)
  
  return(500Data)
}
#   ### Calculate the Indicators for the stock
#   source("myIndicators.R")
#   stockInd <- myIndicators(stockData, stockDataXTS)
#   
#   # preceeding daily, weekly, monthly, quarterly, yearly returns
#   source("myPrecedingReturns.R")
#   preceedingRets <- myPrecedingReturns(stockInd)
# 
#   # preceeding daily, weekly, monthly, quarterly, yearly RSI
#   source("myPrecedingRSI.R")
#   preceedingRSIs <- myPrecedingRSI(stockInd)
# 
#   # # preceeding 10 day average log return
#   # log10day.GILD =  rep(0, as.integer(length(log.GILD)/5))
#   # segment = 1:length(log10day.GILD)
#   # 
#   # for (d in segment){
#   #   log10day.GILD[d] <- mean(log.GILD[((d*5)-4):((d*5)+5)])
#   # }
# 
#   # preceeding 5, 10, 15, 20, yearly SMA (simple moving average)
#   source("myPrecedingSMA.R")
#   preceedingSMAs <- myPrecedingSMA(stockInd)
# 
#   # preceeding 10 days average Oversold
#   oversold10day =  rep(0, as.integer(length(stockInd[,"oversold"])/5))
#   segment = 1:length(oversold10day)
# 
#   for (d in segment){
#     oversold10day[d] <- mean(stockInd$oversold[(((d+2)*5)-9):((d+2)*5)])
#   }
#   oversold10day = append(oversold10day, rep(0, 2), after = 0)
# 
#   # preceeding 10 days average Undersold 
#   undersold10day =  rep(0, as.integer(length(stockInd$undersold)/5))
#   segment = 1:length(undersold10day)
# 
#   for (d in segment){
#     undersold10day[d] <- mean(stockInd$undersold[(((d+2)*5)-9):((d+2)*5)])
#   }
#   undersold10day = append(undersold10day, rep(0, 2), after = 0)
#   
#   source("mergeStockFeatures.R")
#   stockFeaturesDF <- mergeStockFeatures(weeklyClass, oversold10day, undersold10day, preceedingRSIs, preceedingRets, preceedingSMAs)
#   
#   
#   result <- data.frame()
#   return(result)
# }