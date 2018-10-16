library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(caschrono)
library(TSA)
library(quantmod)

all_data <- read.csv("./Time Series/FinalProject/well_rain_tide.csv")

View(all_data)

x.reg <- cbind(all_data$rain, all_data$tide)

View(x.reg)

well_ts <- ts(all_data$well, frequency = 24 * 365.25)

model <- Arima(well_ts, order=c(2,0,0), xreg=x.reg)

ndiffs(model$residuals)

p.val0 <- vector(length=12)
for (i in 3:15){
  p.val0[i-2]=Box.test(model$residuals,lag=i,type='Ljung-Box')$p.value
}
p.val0
Acf(model$residuals)
Pacf(model$residuals)


model1 <- Arima(well_ts, order=c(2,0,12), xreg=x.reg)

ndiffs(model1$residuals)

p.val <- vector(length=12)
for (i in 17:(17+11)){
  p.val[i-16]=Box.test(model1$residuals,lag=i,type='Ljung-Box')$p.value
}
p.val
Acf(model1$residuals)
Pacf(model1$residuals)

model2 <- Arima(well_ts, order=c(2,0,2), xreg=x.reg)

ndiffs(model2$residuals)

p.val2 <- vector(length=12)
for (i in 3:15){
  p.val2[i-2]=Box.test(model2$residuals,lag=i,type='Ljung-Box')$p.value
}
p.val2
Acf(model2$residuals)
Pacf(model2$residuals)

#-----------------------------------------------------------------------------
# running automatic selection
#-----------------------------------------------------------------------------

auto_select_model <- auto.arima(model$residuals, seasonal = F)

best_model <- Arima(well_ts, order = c(2, 0, 2), xreg = all_data$tide)

p.val_best <- vector(length=30)
for (i in 1:30){
  p.val_best[i]=Box.test(best_model$residuals,lag=i,type='Ljung-Box')$p.value
}
p.val_best

Acf(best_model$residuals)
Pacf(best_model$residuals)


