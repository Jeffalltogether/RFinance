### calculates the indicators for a stock using some quantmod functions
### the arguments can be in xts, but i prefer bringing a vector data frame
myMergedIndicators <- function(MergedData, MergedXTS){

#    Replaced with function "allReturns"  
#    # Calculate the daily log returns:
#    log.stockData <- log(stockData$stockData.Close[-1]/stockData$stockData.Close[-length(stockData$stockData.Close)])
#    #llog.stockData = 100*Log.stockData # Transformation so that the numbers are not soooo small
    
    # Calculate the dailry return
    allRet <- allReturns(as.xts(MergedXTS[,1:5]))
    StockdailyR = as.vector(allRet[, "daily"])*100
    StockweeklyR = as.vector(allRet[, "weekly"])*100
    StockmonthlyR = as.vector(allRet[, "monthly"])*100
    StockquarterlyR = as.vector(allRet[, "quarterly"])*100
    StockyearlyR = as.vector(allRet[, "yearly"])*100
   
    SP50allRet <- allReturns(as.xts(MergedXTS[,1:5]))
    SP500dailyR = as.vector(allRet[, "daily"])*100
    SP500weeklyR = as.vector(allRet[, "weekly"])*100
    SP500monthlyR = as.vector(allRet[, "monthly"])*100
    SP500quarterlyR = as.vector(allRet[, "quarterly"])*100
    SP500yearlyR = as.vector(allRet[, "yearly"])*100

    allRet <- allReturns(as.xts(MergedXTS[,1:5]))
    JunkBdailyR = as.vector(allRet[, "daily"])*100
    JunkBweeklyR = as.vector(allRet[, "weekly"])*100
    JunkBmonthlyR = as.vector(allRet[, "monthly"])*100
    JunkBquarterlyR = as.vector(allRet[, "quarterly"])*100
    JunkByearlyR = as.vector(allRet[, "yearly"])*100
    
    # Calculate the Daily Change
    Change = stockData[, "Open"]-stockData[, "Close"]
    
    # Calculate the 10 day SMA
    SMA <- SMA(stockData[, "Close"], 10)
    
    # Calculate the over and under 200 day SMA
    
    # Calculate the over and under the S&P 500 performance
    
    # Calculate the RSI
    RSI <- RSI(stockData[, "Close"])
    
    # Calculate RSI under 30 and over 70
    oversold = RSI
    oversold[oversold < 70] <- 0 # 0 is for times that the RSI is under 70
    oversold[oversold > 70] <- 1 # 1 is for times that the RSI is over 70
    
    undersold = RSI
    undersold[undersold < 30] <- 1 # 1 is for times that the RSI is under 30
    undersold[undersold > 30] <- 0 # 0 is for times that the RSI is over 30
    
    # Calculate the spread of the Daily Change using Boiler Bands
    BoilerB = BBands(stockData[, "Close"])
    
#    # Calculate Lag statistics 1, 5, 10, 20, 50, 100, 200
#     lagList <- c(1, 5, 10, 20, 50, 100, 200) #determine 'k' as number of lags (in units of observation) 
#     lag.stockData=matrix(0, length(stockData),length(lagList)) #generate an empty vector for the loop
#     
#     degree = 1:7
#     for (d in degree){
#       lag.stockData[,d] = lag(stockData[,4], k = lagList[d])  
#     }

    # Generate indicators data frame
    result1 <- data.frame(Change, dailyR, weeklyR, monthlyR, quarterlyR, yearlyR, SMA, RSI, oversold, undersold, BoilerB[, "up"], BoilerB[, "dn"])
    
    return(result1)
}
