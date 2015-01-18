myPrecedingSMA <- function(stockInd){
  # preceeding daily, weekly, monthly, quarterly, yearly Returns
  prev1DaysSMA = rep(0, as.integer(length(stockInd[, "SMA"])/5))
  prev2DaysSMA = prev1DaysSMA
  prev3DaysSMA = prev1DaysSMA
  prev4DaysSMA = prev1DaysSMA
  prev5DaysSMA = prev1DaysSMA
  prev8DaysSMA = prev1DaysSMA
  prev10DaysSMA = prev1DaysSMA
  prev15DaysSMA = prev1DaysSMA
  prev20DaysSMA = prev1DaysSMA
  prev22DaysSMA = prev1DaysSMA 
  prev25DaysSMA = prev1DaysSMA
  #prevQuartersSMA = prev1DaysSMA
  
  segment = 1:length(prev1DaysSMA)
  for (d in segment){
    prev1DaysSMA[d] <- mean(stockInd$SMA[d*5])                           #Spacing is good
    prev2DaysSMA[d] = mean(stockInd$SMA[((d*5)-1):(d*5)])                #Spacing is good
    prev3DaysSMA[d] = mean(stockInd$SMA[((d*5)-2):(d*5)])                #Spacing is good
    prev4DaysSMA[d] = mean(stockInd$SMA[((d*5)-3):(d*5)])                #Spacing is good
    prev5DaysSMA[d] = mean(stockInd$SMA[((d*5)-4):(d*5)])                #Spacing is good
    
    prev8DaysSMA[d] = mean(stockInd$SMA[(((d+2)*5)-7):((d+2)*5)])        #Spacing is off by 2
    prev10DaysSMA[d] = mean(stockInd$SMA[(((d+2)*5)-9):((d+2)*5)])
    
    prev15DaysSMA[d] = mean(stockInd$SMA[(((d+3)*5)-14):((d+3)*5)])      #Spacing is off by 3
    
    prev20DaysSMA[d] = mean(stockInd$SMA[(((d+4)*5)-19):((d+4)*5)])      #Spacing is off by 4
    
    prev22DaysSMA[d] = mean(stockInd$SMA[(((d+5)*5)-21):((d+5)*5)])      #Spacing is off by 5
    prev25DaysSMA[d] = mean(stockInd$SMA[(((d+5)*5)-24):((d+5)*5)])
    
    #prevQuartersSMA[d] = mean(stockInd$SMA[(((d+13)*5)-65):((d+13)*5)])  #Spacing is off by 13; leaving out for now
  }
  prev8DaysSMA = append(prev8DaysSMA, rep(0, 2), after = 0)
  prev10DaysSMA = append(prev10DaysSMA, rep(0, 2), after = 0)
  prev15DaysSMA = append(prev15DaysSMA, rep(0, 3), after = 0)
  prev20DaysSMA = append(prev20DaysSMA, rep(0, 4), after = 0)
  prev22DaysSMA = append(prev22DaysSMA, rep(0, 5), after = 0)
  prev25DaysSMA = append(prev25DaysSMA, rep(0, 5), after = 0)
  #prevQuartersSMA = append(prevQuartersSMA,rep(0, 13), after = 0) # leaving out for now
  
  result1 <- data.frame(prev1DaysSMA[8:length(prev1DaysSMA)], prev2DaysSMA[8:length(prev1DaysSMA)], prev3DaysSMA[8:length(prev1DaysSMA)], prev4DaysSMA[8:length(prev1DaysSMA)], prev5DaysSMA[8:length(prev1DaysSMA)], prev8DaysSMA[8:length(prev1DaysSMA)], prev10DaysSMA[8:length(prev1DaysSMA)], prev15DaysSMA[8:length(prev1DaysSMA)], prev20DaysSMA[8:length(prev1DaysSMA)], prev22DaysSMA[8:length(prev1DaysSMA)],  prev25DaysSMA[8:length(prev1DaysSMA)])
  
  return(result1)
}