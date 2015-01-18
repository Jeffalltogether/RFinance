myPrecedingRSI <- function(stockInd){
  # preceeding daily, weekly, monthly, quarterly, yearly RSI
  prev1DaysRSI = rep(0, as.integer(length(stockInd[, "RSI"])/5))
  prev2DaysRSI = prev1DaysRSI
  prev3DaysRSI = prev1DaysRSI
  prev4DaysRSI = prev1DaysRSI
  prev5DaysRSI = prev1DaysRSI
  prev8DaysRSI = prev1DaysRSI
  prev10DaysRSI = prev1DaysRSI
  prev15DaysRSI = prev1DaysRSI
  prev20DaysRSI = prev1DaysRSI
  prev22DaysRSI = prev1DaysRSI 
  prev25DaysRSI = prev1DaysRSI
  #prevQuartersRSI = prev1DaysRSI
  
  segment = 1:length(prev1DaysRSI)
  for (d in segment){
    prev1DaysRSI[d] <- mean(stockInd$RSI[d*5])                           #Spacing is good
    prev2DaysRSI[d] = mean(stockInd$RSI[((d*5)-1):(d*5)])                #Spacing is good
    prev3DaysRSI[d] = mean(stockInd$RSI[((d*5)-2):(d*5)])                #Spacing is good
    prev4DaysRSI[d] = mean(stockInd$RSI[((d*5)-3):(d*5)])                #Spacing is good
    prev5DaysRSI[d] = mean(stockInd$RSI[((d*5)-4):(d*5)])                #Spacing is good
    
    prev8DaysRSI[d] = mean(stockInd$RSI[(((d+2)*5)-7):((d+2)*5)])        #Spacing is off by 2
    prev10DaysRSI[d] = mean(stockInd$RSI[(((d+2)*5)-9):((d+2)*5)])
    
    prev15DaysRSI[d] = mean(stockInd$RSI[(((d+3)*5)-14):((d+3)*5)])      #Spacing is off by 3
    
    prev20DaysRSI[d] = mean(stockInd$RSI[(((d+4)*5)-19):((d+4)*5)])      #Spacing is off by 4
    
    prev22DaysRSI[d] = mean(stockInd$RSI[(((d+5)*5)-21):((d+5)*5)])      #Spacing is off by 5
    prev25DaysRSI[d] = mean(stockInd$RSI[(((d+5)*5)-24):((d+5)*5)])
    
    #prevQuartersRSI[d] = mean(stockInd$RSI[(((d+13)*5)-65):((d+13)*5)])  #Spacing is off by 13; leaving out for now
  }
  prev8DaysRSI = append(prev8DaysRSI, rep(0, 2), after = 0)
  prev10DaysRSI = append(prev10DaysRSI, rep(0, 2), after = 0)
  prev15DaysRSI = append(prev15DaysRSI, rep(0, 3), after = 0)
  prev20DaysRSI = append(prev20DaysRSI, rep(0, 4), after = 0)
  prev22DaysRSI = append(prev22DaysRSI, rep(0, 5), after = 0)
  prev25DaysRSI = append(prev25DaysRSI, rep(0, 5), after = 0)
  #prevQuartersRSI = append(prevQuartersRSI,rep(0, 13), after = 0) # leaving out for now
  
  result1 <- data.frame(prev1DaysRSI[8:length(prev1DaysRSI)], prev2DaysRSI[8:length(prev1DaysRSI)], prev3DaysRSI[8:length(prev1DaysRSI)], prev4DaysRSI[8:length(prev1DaysRSI)], prev5DaysRSI[8:length(prev1DaysRSI)], prev8DaysRSI[8:length(prev1DaysRSI)], prev10DaysRSI[8:length(prev1DaysRSI)], prev15DaysRSI[8:length(prev1DaysRSI)], prev20DaysRSI[8:length(prev1DaysRSI)], prev22DaysRSI[8:length(prev1DaysRSI)],  prev25DaysRSI[8:length(prev1DaysRSI)])
  
  return(result1)
}