library(jsonlite)
library(qrmtools)
APIstock <- "https://api.polygon.io/v2/aggs/ticker/O:SPWX221104C0"
APItid <- "/range/1/minute/2022-10-04/2022-11-03?adjusted=true&sort=asc&limit=50000&api"
APIkey <- "Key=CBQouP6k8C92g2XWhofZB5PCGgxI6lOk"

startprice <- 100
#3639
#3878

API<- paste(APIstock,as.character(as.integer(startprice*1000)),APItid, APIkey, sep = "")
raw <- jsonlite::fromJSON(API)

vw <- raw$results$vw
t <- raw$results$t

df0 <- data.frame(vw,t)


for (i in (startprice+1):355) {
K <- i*1000
API<- paste(APIstock,as.character(as.integer(K)),APItid, APIkey, sep = "") 

raw <- jsonlite::fromJSON(API)
vw <- raw$results$vw
t <- raw$results$t
df <- data.frame(vw,t)

df0 <- merge(df0, df, by = "t", all = TRUE)
}

