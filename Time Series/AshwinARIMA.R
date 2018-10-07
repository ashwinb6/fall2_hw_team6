library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(tseries)

well_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\well_agg_with_impute.csv")

#making ts object
well_ts = ts(well_data$Corrected_w_imputations, start = c(2007,10), frequency = 24*365.25)

#decom
decomp= stl(well_ts, s.window=7)
plot(decomp)


#stationary after taking differences
adf.test(well_ts, alternative=c("stationary"), k=2)
well_diff = diff(well_ts, differences=1)
adf.test(well_diff, alternative = c("stationary"), k=2)
plot(well_ts)
plot(well_diff)

#training and holdout
well_station=subset(well_diff, end=length(well_ts)-169)
#holdout made from 24*7(last week of well data)
well_holdout=subset(well_diff,start=length(well_ts)-168)


#this decomp plot looks really odd??
decomp= stl(well_station, s.window=7)
plot(decomp)

acf(well_station, lag=60)
pacf(well_station,lag=60)

auto = auto.arima(well_station)
# ARIMA(1,0,2)(2,0,1)[24] with zero mean 
# 
# Coefficients:
#   ar1      ma1     ma2    sar1    sar2     sma1
# 0.7202  -0.3276  0.0618  0.4860  0.0214  -0.4042
# s.e.  0.0054   0.0062  0.0044  0.0348  0.0052   0.0347
# 
# sigma^2 estimated as 9.516e-05:  log likelihood=301163.6
# AIC=-602313.3   AICc=-602313.3   BIC=-602247.1
# 
# Training set error measures:
#   ME        RMSE         MAE MPE MAPE      MASE          ACF1
# Training set 1.102839e-06 0.009754908 0.005389747 NaN  Inf 0.8030551 -6.659348e-05
summary(auto)
acf(auto$residuals, lag=40)
pacf(auto$residuals, lag=40)


well_arima = Arima(well_station, order=c(1, 0, 2), seasonal = c(2, 0, 1), method="ML")
summary(well_arima)
acf(well_arima$residuals, lag=60)
pacf(well_arima$residuals, lag=60)

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(arima.trig$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

arima.trig<-Arima(well_station,order=c(4,0,3),xreg=fourier(well_station,K=4))   #fourier is a combination of fitting sines and cosines
# K= _ means how many sine/cosine terms you want
# summary(arima.trig)
acf(arima.trig$residuals, lag=40)
pacf(arima.trig$residuals, lag=40)

plot(arima.trig$residuals)

           