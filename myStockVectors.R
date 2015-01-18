myStockVectors <- function(stockDataXTS){
  ### Calculate Indicators for One Stock
  Open = as.vector(stockDataXTS[, "stockDataXTS.Open"]) 
  High = as.vector(stockDataXTS[, "stockDataXTS.High"]) 
  Low = as.vector(stockDataXTS[, "stockDataXTS.Low"]) 
  Close = as.vector(stockDataXTS[, "stockDataXTS.Close"])
  Volume = as.vector(stockDataXTS[, "stockDataXTS.Volume"])
  result1 <- data.frame(Open, High, Low, Close, Volume)
  return(result1)
}