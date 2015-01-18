mergeStockFeatures <- function(StockClasses, StockOverUnder, StockPrevRSIs, StockPrevRets, StockPrevSMAs){
  
  stockFeaturesDF <- data.frame(StockClasses[8:length(StockClasses)], StockOverUnder[8:(nrow(StockOverUnder)-2),], StockPrevRSIs, StockPrevRets, StockPrevSMAs)
  stockFeaturesDF = rename(stockFeaturesDF, 
                      c("StockClasses.8.length.StockClasses.."="weeklyClass",              "prev1DaysRSI.8.length.prev1DaysRSI.."="prev1DaysRSI",           
                        "prev2DaysRSI.8.length.prev1DaysRSI.."="prev2DaysRSI",             "prev3DaysRSI.8.length.prev1DaysRSI.."="prev3DaysRSI",          
                        "prev4DaysRSI.8.length.prev1DaysRSI.."="prev4DaysRSI",             "prev5DaysRSI.8.length.prev1DaysRSI.."="prev5DaysRSI",          
                        "prev8DaysRSI.8.length.prev1DaysRSI.."="prev8DaysRSI",             "prev10DaysRSI.8.length.prev1DaysRSI.."="prev10DaysRSI",         
                        "prev15DaysRSI.8.length.prev1DaysRSI.."="prev15DaysRSI",           "prev20DaysRSI.8.length.prev1DaysRSI.."="prev20DaysRSI",         
                        "prev22DaysRSI.8.length.prev1DaysRSI.."="prev22DaysRSI",           "prev25DaysRSI.8.length.prev1DaysRSI.."="prev25DaysRSI",         
                        "prev1DaysRet.8.length.prev1DaysRet.."="prev1DaysRet",             "prev2DaysRet.8.length.prev1DaysRet.."="prev2DaysRet",          
                        "prev3DaysRet.8.length.prev1DaysRet.."="prev3DaysRet",             "prev4DaysRet.8.length.prev1DaysRet.."="prev4DaysRet",          
                        "prev5DaysRet.8.length.prev1DaysRet.."="prev5DaysRet",             "prev8DaysRet.8.length.prev1DaysRet.."="prev8DaysRet",          
                        "prev10DaysRet.8.length.prev1DaysRet.."="prev10DaysRet",           "prev15DaysRet.8.length.prev1DaysRet.."="prev15DaysRet",         
                        "prev20DaysRet.8.length.prev1DaysRet.."="prev20DaysRet",           "prev22DaysRet.8.length.prev1DaysRet.."="prev22DaysRet",         
                        "prev25DaysRet.8.length.prev1DaysRet.."="prev25DaysRet",           "prev1DaysSMA.8.length.prev1DaysSMA.."="prev1DaysSMA",          
                        "prev2DaysSMA.8.length.prev1DaysSMA.."="prev2DaysSMA",             "prev3DaysSMA.8.length.prev1DaysSMA.."="prev3DaysSMA",          
                        "prev4DaysSMA.8.length.prev1DaysSMA.."="prev4DaysSMA",             "prev5DaysSMA.8.length.prev1DaysSMA.."="prev5DaysSMA",          
                        "prev8DaysSMA.8.length.prev1DaysSMA.."="prev8DaysSMA",             "prev10DaysSMA.8.length.prev1DaysSMA.."="prev10DaysSMA",         
                        "prev15DaysSMA.8.length.prev1DaysSMA.."="prev15DaysSMA",           "prev20DaysSMA.8.length.prev1DaysSMA.."="prev20DaysSMA",         
                        "prev22DaysSMA.8.length.prev1DaysSMA.."="prev22DaysSMA",           "prev25DaysSMA.8.length.prev1DaysSMA.."="prev25DaysSMA"
                      ))
  return(stockFeaturesDF)
}