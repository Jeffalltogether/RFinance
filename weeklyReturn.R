### Generate Classes from Stock Time Series Vector
### Classes are the Percent [%] 5-day return

weeklyReturn <- function(stockVector){
  Close = stockVector[, "Close"]
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
  
  weeklyClass=na.omit(weeklyClass)
  
  return(weeklyClass)
}