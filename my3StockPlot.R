my3StockPlot <- function (Ticker1, Ticker2, Ticker3){
  Ticker1 = "BRK.B"       # <<-------------------------------- enter stock name here
  Ticker2 = "OAK"        # <<-------------------------------- enter stock name here
  Ticker3 = "ORCL"       # <<-------------------------------- enter stock name here
  
  varList <- c(Ticker1, Ticker2, Ticker3)
  
  xtsList=rep(0,length(varList))
  
  degree = 1:3
for (d in degree){
  getSymbols(varList[d], src = "google") # src = "yahoo" is the default
  # Candlestick Data Visualization
  candleChart(get(varList[d]), subset='201407::201412/', name = varList[d], theme='white',TA=NULL) #yyyymmdd::yyyymmdd (start time :: end time)
  addVo()
  addRSI()
  addBBands()
  addMACD(32,50,12)
}
return()
}