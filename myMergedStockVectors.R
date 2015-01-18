myMergedStockVectors <- function(MergedXTS){
  ### Calculate Indicators for One Stock
  StockOpen = as.vector(MergedXTS[, "stockDataXTS.Open"]) 
  StockHigh = as.vector(MergedXTS[, "stockDataXTS.High"]) 
  StockLow = as.vector(MergedXTS[, "stockDataXTS.Low"]) 
  StockClose = as.vector(MergedXTS[, "stockDataXTS.Close"])
  StockVolume = as.vector(MergedXTS[, "stockDataXTS.Volume"])
  
  SP500Open = as.vector(MergedXTS[, "SP500.Open"]) 
  SP500High = as.vector(MergedXTS[, "SP500.High"]) 
  SP500Low = as.vector(MergedXTS[, "SP500.Low"]) 
  SP500Close = as.vector(MergedXTS[, "SP500.Close"])
  SP500Volume = as.vector(MergedXTS[, "SP500.Volume"])
  
  JunkBOpen = as.vector(MergedXTS[, "JunkB.Open"]) 
  JunkBHigh = as.vector(MergedXTS[, "JunkB.High"]) 
  JunkBLow = as.vector(MergedXTS[, "JunkB.Low"]) 
  JunkBClose = as.vector(MergedXTS[, "JunkB.Close"])
  JunkBVolume = as.vector(MergedXTS[, "JunkB.Volume"])
  
  result1 <- data.frame(StockOpen, StockHigh, StockLow, StockClose, StockVolume, SP500Open, SP500High, SP500Low, SP500Close, SP500Volume, JunkBOpen, JunkBHigh, JunkBLow, JunkBClose, JunkBVolume)
  return(result1)
}