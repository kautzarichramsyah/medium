library(forecast)
library(ggplot2)

# Load the data
data <- AirPassengers

# naive
fit <- naive(data, h=12)
autoplot(fit) + ggtitle("Naive Forecast") + xlab("Year") + ylab("Passengers")

# moving average 
fit <- ma(data, order=12)
data_combined <- cbind(data, fit)
autoplot(data_combined) + ggtitle("Moving Average Forecast") + xlab("Year") + ylab("Passengers")

# Simple Exponential Smoothing
fit <- ses(data, h = 12)
autoplot(fit) + ggtitle("Simple Exponential Smoothing Forecast") + xlab("Year") + ylab("Passengers")

# Holt's Linear Trend Model
fit <- holt(data, h=12)
autoplot(fit) + ggtitle("Holt's Linear Trend Forecast") + xlab("Year") + ylab("Passengers")

# Holt-Winters Seasonal Model
fit <- hw(data, seasonal = "multiplicative", h = 12)
autoplot(fit) + ggtitle("Holt-Winters Seasonal Model Forecast") + xlab("Year") + ylab("Passengers")

# ARIMA model
fit <- auto.arima(data, seasonal = FALSE)
forecasted <- forecast(fit, h = 12)
autoplot(forecasted) + ggtitle("ARIMA Forecast") + xlab("Year") + ylab("Passengers")

# Fit SARIMA model
fit <- auto.arima(data)
forecasted <- forecast(fit, h = 12)
autoplot(forecasted) + ggtitle("SARIMA Forecast") + xlab("Year") + ylab("Passengers")
