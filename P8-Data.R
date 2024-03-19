library(jsonlite)
library(roptions)
#apiKey=CBQouP6k8C92g2XWhofZB5PCGgxI6lOk
API<-"https://api.polygon.io/v2/aggs/ticker/O:SPY221104C00374000/range/1/day/2022-10-04/2022-11-03?adjusted=true&sort=asc&limit=360&apiKey=CBQouP6k8C92g2XWhofZB5PCGgxI6lOk"

raw <- jsonlite::fromJSON(API)
Data <- raw$results
plot(Data$vw)
