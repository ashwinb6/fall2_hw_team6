library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)

well_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\well_agg_with_impute.csv")

#making ts object
well_ts = ts(well_data$Corrected_w_imputations, start = c(2007,10), frequency = 24)

#decom
decomp= stl(well_ts, s.window=7)
plot(decomp)

#training and holdout
well_train=subset(well_ts, end=length(well_ts)-169)
  #holdout made from 24*7(last week of well data)
well_holdout=subset(well_ts,start=length(well_ts)-168)

#stationary after taking differences
adf.test(well_ts, alternative=c("stationary"), k=2)
well_station = diff(well_ts, differences=1)
adf.test(well_station, alternative = c("stationary"), k=2)
plot(well_ts)
plot(well_station)


#this decomp plot looks really odd??
decomp= stl(well_station, s.window=7)
plot(decomp)

acf(well_station, lag=40)
pacf(well_station,lag=40)

auto = auto.arima(diff(well_station, 24))
#Output: Series: diff(well_station, 12) 
# ARIMA(5,0,0)(2,0,0)[24] with zero mean 
# 
# Coefficients:
#   ar1     ar2     ar3     ar4      ar5    sar1    sar2
# 0.3389  0.1667  0.0479  0.0121  -0.0067  0.0382  0.0365
# s.e.     NaN  0.0030     NaN  0.0025      NaN  0.0038     NaN
# 
# sigma^2 estimated as 0.0001781:  log likelihood=271739.4
# AIC=-543462.8   AICc=-543462.8   BIC=-543387.2
summary(auto)


well_arima = arima(well_station, order=c(5, 0, 0), seasonal=c(2, 0, 0), method="ML") #Why isn't it running seasonal
# Call:
#   arima(x = well_station, order = c(1, 0, 0))
# 
# Coefficients:
#   ar1  intercept
# 0.5144      0e+00
# s.e.  0.0028      1e-04
# 
# sigma^2 estimated as 0.0001004:  log likelihood = 298627.6,  aic = -597249.3
# 
# Training set error measures:
#   ME      RMSE         MAE MPE MAPE      MASE       ACF1
# Training set -5.765189e-08 0.0100223 0.005343962 NaN  Inf 0.8283618 -0.1037829
summary(well_arima)

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(well_arima$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")







           