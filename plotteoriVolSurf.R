library(plotly)

# Havde problemer med quant pakken vi brugte sidste semester 
# til at hente option prices fordi der var noget GDPR gøjl jeg ikke har kunne fikse
# derfor har jeg bare lavet til teori plottet

# vi laver function der mapper vores strike price og maturity time 
generate_volatility_surface <- function(strike_prices, time_to_maturity) {
  # sidste del er function vi kan tilpasse så den ser ordenlig ud
  implied_volatility <- outer(strike_prices, time_to_maturity, FUN = function(x, y) 0.1 * x^0.1 + 0.02 * y^0.1)
  return(implied_volatility)
}

# strike og maturity
strike_prices <- seq(80, 120, by = 2)
time_to_maturity <- seq(0.1, 2, by = 0.1)

# vi laver vores y værdier
volatility_surface <- generate_volatility_surface(strike_prices, time_to_maturity)

# 3D plot 
plot_ly(
  x = strike_prices,
  y = time_to_maturity,
  z = volatility_surface,
  type = "surface"
) %>%
  layout(
    title = "Volatility Surface",
    scene = list(
      xaxis = list(title = "Strike Price"),
      yaxis = list(title = "Time to Maturity"),
      zaxis = list(title = "Implied Volatility")
    )
  )
