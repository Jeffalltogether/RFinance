### Generate the features related to the S&P 500 as the SPY Index Funds
SPYMetrics <- function(otherMetric){
  
  setwd("C:/Users/jeffthatcher/Cloud Drive/Documents/R/StockAnalysis")
  ### Use single quotes and specify data source:
  Ticker1 = "SPY"       # <<-------------------------------- enter stock name here
  
  ### Download Stock Data
  getSymbols(Ticker1, src = "google")  # src = "yahoo" is the default
  
  ### Re-name an xts and replace all the headers
  stockDataXTS = SPY         # <<------------------------------- enter stock name here
  stockColNames <- sub(Ticker1, "stockDataXTS", names(stockDataXTS))
  names(stockDataXTS) = stockColNames # 13 characters shorter. 
  stockDataXTS=na.omit(stockDataXTS)
  
  ### Convert raw stock data into a vector data frame
  source("myStockVectors.R")
  stockData <- myStockVectors(stockDataXTS)
  
  ### Calculate the Indicators for the stock
  source("myIndicators.R")
  stockInd <- myIndicators(stockData, stockDataXTS)

  ### Create Classes to Predict
  # calcualte the percent [%] five (5) day return
  Close = stockData[, "Close"]
  fivedayPerRet =  rep(0, as.integer(length(Close)/5))
  segment = 1:length(fivedayPerRet)
  
  for (d in segment){
    #Start with second week in the series; (day 10 - day 5)/ day 5
    fivedayPerRet[d] = ((Close[(d+1)*5]-Close[((d*5)+1)])/Close[((d*5)+1)])*100   
  }
  
  # Sort five day return into categories; CLASSES
  
  weeklyClass = fivedayPerRet
  weeklyClass[weeklyClass > 6] <- 'A'                       # A 
  weeklyClass[weeklyClass > 0 & weeklyClass <= 6 ] <- 'B'    # B
  weeklyClass[weeklyClass > -6 & weeklyClass <= 0] <- 'C'    # C
  weeklyClass[weeklyClass <= -6] <- 'D'                     # D
  
  weeklyClass[weeklyClass == 'A'] <- 4                      # A 
  weeklyClass[weeklyClass == 'B'] <- 3                      # B
  weeklyClass[weeklyClass == 'C'] <- 2                      # C
  weeklyClass[weeklyClass == 'D'] <- 1                      # D
  
  weeklyClass = append(weeklyClass, rep(0, 1), after = 0)
  weeklyClass = as.integer(weeklyClass)
  
  # preceeding daily, weekly, monthly, quarterly, yearly returns
  source("myPrecedingReturns.R")
  preceedingRets <- myPrecedingReturns(stockInd)
  
  # preceeding daily, weekly, monthly, quarterly, yearly RSI
  source("myPrecedingRSI.R")
  preceedingRSIs <- myPrecedingRSI(stockInd)
  
  # preceeding 5, 10, 15, 20, yearly SMA (simple moving average)
  source("myPrecedingSMA.R")
  preceedingSMAs <- myPrecedingSMA(stockInd)
  
  # preceeding 10 days average Oversold
  oversold10day =  rep(0, as.integer(length(stockInd[,"oversold"])/5))
  segment = 1:length(oversold10day)
  
  for (d in segment){
    oversold10day[d] <- mean(stockInd$oversold[(((d+2)*5)-9):((d+2)*5)])
  }
  oversold10day = append(oversold10day, rep(0, 2), after = 0)
  
  # preceeding 10 days average Undersold 
  undersold10day =  rep(0, as.integer(length(stockInd$undersold)/5))
  segment = 1:length(undersold10day)
  
  for (d in segment){
    undersold10day[d] <- mean(stockInd$undersold[(((d+2)*5)-9):((d+2)*5)])
  }
  undersold10day = append(undersold10day, rep(0, 2), after = 0)

  ### Ground Truth Dataframe
  stockFeaturesDF <- data.frame(weeklyClass[8:length(fivedayPerRet)], oversold10day[8:(length(oversold10day)-2)], undersold10day[8:(length(undersold10day)-2)], preceedingRSIs, preceedingRets, preceedingSMAs)
  stockFeaturesDF = rename(stockFeaturesDF, 
                           c("weeklyClass.8.length.fivedayPerRet.."="weeklyClass",           "oversold10day.8..length.oversold10day....2.."="oversold10day",  
                             "undersold10day.8..length.undersold10day....2.."="undersold10day", "prev1DaysRSI.8.length.prev1DaysRSI.."="prev1DaysRSI",         
                             "prev2DaysRSI.8.length.prev1DaysRSI.."="prev2DaysRSI",             "prev3DaysRSI.8.length.prev1DaysRSI.."="prev3DaysRSI",          
                             "prev4DaysRSI.8.length.prev1DaysRSI.."="prev4DaysRSI",             "prev5DaysRSI.8.length.prev1DaysRSI.."="prev5DaysRSI",          
                             "prev8DaysRSI.8.length.prev1DaysRSI.."="prev8DaysRSI",             "prev10DaysRSI.8.length.prev1DaysRSI.."="prev10DaysRSI",         
                             "prev15DaysRSI.8.length.prev1DaysRSI.."="prev15DaysRSI",            "prev20DaysRSI.8.length.prev1DaysRSI.."="prev20DaysRSI",         
                             "prev22DaysRSI.8.length.prev1DaysRSI.."="prev22DaysRSI",            "prev25DaysRSI.8.length.prev1DaysRSI.."="prev25DaysRSI",         
                             "prev1DaysRet.8.length.prev1DaysRet.."="prev1DaysRet",           "prev2DaysRet.8.length.prev1DaysRet.."="prev2DaysRet",          
                             "prev3DaysRet.8.length.prev1DaysRet.."="prev3DaysRet",           "prev4DaysRet.8.length.prev1DaysRet.."="prev4DaysRet",          
                             "prev5DaysRet.8.length.prev1DaysRet.."="prev5DaysRet",           "prev8DaysRet.8.length.prev1DaysRet.."="prev8DaysRet",          
                             "prev10DaysRet.8.length.prev1DaysRet.."="prev10DaysRet",          "prev15DaysRet.8.length.prev1DaysRet.."="prev15DaysRet",         
                             "prev20DaysRet.8.length.prev1DaysRet.."="prev20DaysRet",          "prev22DaysRet.8.length.prev1DaysRet.."="prev22DaysRet",         
                             "prev25DaysRet.8.length.prev1DaysRet.."="prev25DaysRet",          "prev1DaysSMA.8.length.prev1DaysSMA.."="prev1DaysSMA",          
                             "prev2DaysSMA.8.length.prev1DaysSMA.."="prev2DaysSMA",           "prev3DaysSMA.8.length.prev1DaysSMA.."="prev3DaysSMA",          
                             "prev4DaysSMA.8.length.prev1DaysSMA.."="prev4DaysSMA",           "prev5DaysSMA.8.length.prev1DaysSMA.."="prev5DaysSMA",          
                             "prev8DaysSMA.8.length.prev1DaysSMA.."="prev8DaysSMA",           "prev10DaysSMA.8.length.prev1DaysSMA.."="prev10DaysSMA",         
                             "prev15DaysSMA.8.length.prev1DaysSMA.."="prev15DaysSMA",          "prev20DaysSMA.8.length.prev1DaysSMA.."="prev20DaysSMA",         
                             "prev22DaysSMA.8.length.prev1DaysSMA.."="prev22DaysSMA",          "prev25DaysSMA.8.length.prev1DaysSMA.."="prev25DaysSMA"
                           ))
  
  return(FeaturesDF)
}