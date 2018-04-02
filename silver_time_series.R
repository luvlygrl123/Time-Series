library(fpp2)
library(astsa)
library(tseries)
library(forecast)
library(readxl)
library(xts)
library(dplyr)
library(prophet)

#Step 1 Read and visualize data; split train/test
#Step 2 Stationarize data
#Step 3 Plot ACF and PACF
#Compare the AIC and the BIC; lowest AIC and BIC
#Step 4 Build model
#Step 5 # Step 5 Forecast
#Holt
#Prophet

setwd("C:/Users/Suzannah/Desktop/Spring 2018/DATA 902/Time Series")
silver_1 <- read_excel("Silver Prices.xlsx")

silver_time <- ts(silver_1$Price, start = c(2008, 1), frequency = 12)


logst <- log(silver_time)
logst
train <- window(logst, c(2008,1), c(2017,1))
test <- window(logst, c(2017,2), c(2018,1))

plot(train)

#Its an AR model, even though it doesn't look it at first, the ACF Tails Off
acf2(train)
adf.test(train)
auto.arima(train)

#Compare the AIC and the BIC
fit1 <- sarima(train, p = 0, d = 1, q = 1)
fit2 <- sarima(train, p = 1, d = 1, q = 1)
fit3 <- sarima(train, p = 2, d = 1, q = 1)
fit4 <- sarima(train, p = 2, d = 1, q = 2)

#Fit 3 Has the lowest combination of AIC and BIC
fit3$ttable

sarima_silver <- as.ts(sarima.for(train, n.ahead = 12, p = 2, d = 1, q = 2))
pred_silver <- sarima_silver$pred
acc_1 <- accuracy(pred_silver, test)
acc_1

#############################################################################################
# Holt returns best prediciton of this data

fit <- holt(train)
checkresiduals(fit)

forecast_fit <- forecast(fit, h <- 12)
plot(forecast_fit)
lines(test,col="green")
acc_2 <- accuracy(forecast_fit,test)
acc_2

#############################################################################################
silver_2 <- read.csv("Silver Prices.csv") %>% mutate(Price = log(silver_1$Price))
silver_d <- data.frame(ds = seq(as.Date('2008-01-01'), as.Date('2018-01-01'), by = "month"), y = silver_2$Price)
silver_d
train <- silver_d[1:109,]
test <- silver_d[110:121,]

silver_prophet <- prophet(train)
class(silver_prophet)
silver_future <- make_future_dataframe(silver_prophet, periods = 12, freq = "month")
class(silver_future)
forecasting <- predict(silver_prophet,silver_future)
forecasting
head(forecasting[c('ds', 'yhat')])

testing <- forecasting[c('ds','yhat')]

library(Metrics)
acc_3 <- rmse(test$y, testing[110:121,'yhat'])






