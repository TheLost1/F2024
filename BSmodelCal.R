# Load required packages
library(RQuantLib)

r = 0.04

type = "call"

#load data
P <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/closeP-Apple.csv")
call <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/CallApple.csv")
callPR <- read.csv("/Users/christiantaulbjerg/Documents/GitHub/F2024/Data_w_S_and_r.csv")

# Function to calculate Black-Scholes price
bs_price <- function(sigma, S, K, T, r, option_type) {
  price <- EuropeanOption(type = option_type, underlying = S, strike = K, 
                          dividendYield = 0, riskFreeRate = r, maturity = T, 
                          volatility = sigma)
  return(price)
}

# Function to calculate sum of squared errors
error_function <- function(sigma, observed_price, S, K, T, r, option_type) {
  model_price <- bs_price(sigma, S, K, T, r, option_type)
  error <- (observed_price - model_price$value)^2
  return(error)
}





#bs_price(-0.0566, 138.88, 165, 0.2, 0.425, "call")

# Load your dataset
# Let's assume your dataset is stored in 'options_data' dataframe
# It contains columns: S0, K, T, r, observed_price, option_type

# Create an empty vector to store calibrated volatilities
calibrated_volatilities <- numeric(0)


  

totaltid <-callPR[nrow(callPR),3] - callPR[1,3]


# Iterate over each row of the dataset
for (i in 1:nrow(callPR)) {
  for (j in 4:(ncol(callPR)-1)){
    if (!is.na(callPR[i,j])) {
      S <- callPR[i,ncol(callPR)]
      K <- as.numeric(gsub("\\D", "", colnames(callPR[j]) ))
      T <- (1/12)*(1-(callPR[i,3]-callPR[1,3])/totaltid)
      r <- 0.0425
      obs_price <- callPR[i,j]
      vol_0 <- 0.1
      calib_vol <- optim(vol_0, error_function, 
                         observed_price = obs_price, 
                         S = S, K = K, T = T, r = r, 
                         option_type = "call")$par
      calibrated_volatilities <- c(calibrated_volatilities, calib_vol)
    }
  }
}


mean(abs(calibrated_volatilities))

min(calibrated_volatilities)

#AAPL221104C00

#first day 4 oktober 2022

#last day 3 november 2022

# T = 4 novemeber


