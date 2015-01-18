myPrecedingReturns <- function(stockInd){
  # preceeding daily, weekly, monthly, quarterly, yearly Returns
  prev1DaysRet = rep(0, as.integer(length(stockInd[, "dailyR"])/5))
  prev2DaysRet = prev1DaysRet
  prev3DaysRet = prev1DaysRet
  prev4DaysRet = prev1DaysRet
  prev5DaysRet = prev1DaysRet
  prev8DaysRet = prev1DaysRet
  prev10DaysRet = prev1DaysRet
  prev15DaysRet = prev1DaysRet
  prev20DaysRet = prev1DaysRet
  prev22DaysRet = prev1DaysRet 
  prev25DaysRet = prev1DaysRet
  #prevQuartersRet = prev1DaysRet
  
  segment = 1:length(prev1DaysRet)
  for (d in segment){
    prev1DaysRet[d] <- mean(stockInd$dailyR[d*5])                           #Spacing is good
    prev2DaysRet[d] = mean(stockInd$dailyR[((d*5)-1):(d*5)])                #Spacing is good
    prev3DaysRet[d] = mean(stockInd$dailyR[((d*5)-2):(d*5)])                #Spacing is good
    prev4DaysRet[d] = mean(stockInd$dailyR[((d*5)-3):(d*5)])                #Spacing is good
    prev5DaysRet[d] = mean(stockInd$dailyR[((d*5)-4):(d*5)])                #Spacing is good

    prev8DaysRet[d] = mean(stockInd$dailyR[(((d+2)*5)-7):((d+2)*5)])        #Spacing is off by 2
    prev10DaysRet[d] = mean(stockInd$dailyR[(((d+2)*5)-9):((d+2)*5)])
    
    prev15DaysRet[d] = mean(stockInd$dailyR[(((d+3)*5)-14):((d+3)*5)])      #Spacing is off by 3
    
    prev20DaysRet[d] = mean(stockInd$dailyR[(((d+4)*5)-19):((d+4)*5)])      #Spacing is off by 4
    
    prev22DaysRet[d] = mean(stockInd$dailyR[(((d+5)*5)-21):((d+5)*5)])      #Spacing is off by 5
    prev25DaysRet[d] = mean(stockInd$dailyR[(((d+5)*5)-24):((d+5)*5)])
    
    #prevQuartersRet[d] = mean(stockInd$dailyR[(((d+13)*5)-65):((d+13)*5)])  #Spacing is off by 13; leaving out for now
  }
  prev8DaysRet = append(prev8DaysRet, rep(0, 2), after = 0)
  prev10DaysRet = append(prev10DaysRet, rep(0, 2), after = 0)
  prev15DaysRet = append(prev15DaysRet, rep(0, 3), after = 0)
  prev20DaysRet = append(prev20DaysRet, rep(0, 4), after = 0)
  prev22DaysRet = append(prev22DaysRet, rep(0, 5), after = 0)
  prev25DaysRet = append(prev25DaysRet, rep(0, 5), after = 0)
  #prevQuartersRet = append(prevQuartersRet,rep(0, 13), after = 0) # leaving out for now
  
    result1 <- data.frame(prev1DaysRet[8:length(prev1DaysRet)], prev2DaysRet[8:length(prev1DaysRet)], prev3DaysRet[8:length(prev1DaysRet)], prev4DaysRet[8:length(prev1DaysRet)], prev5DaysRet[8:length(prev1DaysRet)], prev8DaysRet[8:length(prev1DaysRet)], prev10DaysRet[8:length(prev1DaysRet)], prev15DaysRet[8:length(prev1DaysRet)], prev20DaysRet[8:length(prev1DaysRet)], prev22DaysRet[8:length(prev1DaysRet)],  prev25DaysRet[8:length(prev1DaysRet)])
      
  return(result1)
}
    