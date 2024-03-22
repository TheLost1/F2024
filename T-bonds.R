#library("PerformanceAnalytics")
library(quantmod)

getSymbols("DGS3MO", src = "FRED")

tail(DGS3MO)

#download.RiskFree(start = "1998-01-01", end = NULL, compression=c("m","d"))
#rf = download.RiskFree(start = "1997-12-01", end = "2007-01-01")
#class(rf)
#head(rf)
#download.