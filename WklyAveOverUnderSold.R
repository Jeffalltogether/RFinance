### Calculate the weekly average of the Oversold and Undersold stock indicator vectors
# based on Relative Strength Index (RSI)
# Oversold = RSI > 70
# Undersold = RSI < 30
WklyAveOverUnderSold <- function(stockVector){

  oversold10day =  rep(0, as.integer(length(stockVector[,"oversold"])/5))
  segment = 1:length(oversold10day)

  for (d in segment){
    oversold10day[d] <- mean(stockVector$oversold[(((d+2)*5)-9):((d+2)*5)])
  }
  oversold10day = append(oversold10day, rep(0, 2), after = 0)
  
  # preceeding 10 days average Undersold 
  undersold10day =  rep(0, as.integer(length(stockVector$undersold)/5))
  segment = 1:length(undersold10day)
  
  for (d in segment){
    undersold10day[d] <- mean(stockVector$undersold[(((d+2)*5)-9):((d+2)*5)])
  }
  undersold10day = append(undersold10day, rep(0, 2), after = 0)
  
  AveUnderOverSold <- data.frame(oversold10day, undersold10day)
  
  return(AveUnderOverSold)
  
}
