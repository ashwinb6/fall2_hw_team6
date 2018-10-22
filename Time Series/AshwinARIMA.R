library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(tseries)

well_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\well_agg_with_impute.csv")
rain_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\FinalProject\\hourly_rain2.csv")
tide_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\FinalProject\\final_tide.csv")
dim(well_data)
dim(rain_data)
dim(tide_data)

rain_station=subset(rain_data, end=length(rain_ts)-169)
rain_holdout=subset(rain_data, start=length(rain_ts)-168)
tide_station=subset(tide_data, end=length(tide_ts)-169)
tide_holdout=subset(tide_data, start=length(tide_ts)-168)

rain = as.vector(rain_station[,2])
tide = as.vector(tide_station[,1])
x.reg=cbind(rain,tide)

model1=Arima(well_ts,order=c(2,0,0),xreg=x.reg)
ndiffs(model1$residuals)
res.model1 = model1$residuals
auto1 = auto.arima(res.model1,seasonal=T)
# Series: res.model1 
# ARIMA(2,0,3) with zero mean 
# 
# Coefficients:
#   ar1      ar2      ma1     ma2      ma3
# 1.2081  -0.2440  -1.3178  0.5002  -0.1184
# s.e.  0.0376   0.0348   0.0374  0.0382   0.0062
# 
# sigma^2 estimated as 9.445e-05:  log likelihood=301518
# AIC=-603024   AICc=-603024   BIC=-602967.3
# 
# Training set error measures:
#                      ME        RMSE         MAE      MPE     MAPE     MASE         ACF1
# Training set 1.049924e-06 0.009718486 0.005442875 4986.919 5117.863 0.623702 0.0004630211
acf(auto1$residuals)
pacf(auto1$residuals)

model2= Arima(well_ts, order=c(2, 0, 10), xreg=x.reg) #MAPE = .16
acf(model2$residuals)
pacf(model2$residuals)

model3= Arima(well_ts, order=c(4, 0, 6), xreg=x.reg)
summary(model3)


p.val=vector(length=20)
for (i in 1:20)
{p.val[i-2]=Box.test(auto1$residuals,lag=i,type='Ljung-Box')$p.value}
p.val


#making ts object
well_ts = ts(well_data$Corrected_w_imputations, start = c(2007,10), frequency = 24*365.25)
rain_ts = ts(rain, start=c(2007,10), frequency=24*365.25)
tide_ts = ts(tide, start=c(2007,10), frequency=24*365.25)

#decom
decomp= stl(well_ts, s.window=7)
plot(decomp)


#stationary after taking differences
adf.test(well_ts, alternative=c("stationary"), k=2)
well_diff = diff(well_ts, differences=1)
adf.test(well_diff, alternative = c("stationary"), k=2)
plot(well_ts)
plot(well_diff)


#training(called well_station) and holdout
well_station=subset(well_diff, end=length(well_ts)-169)
#holdout made from 24*7(last week of well data)
well_holdout=subset(well_diff,start=length(well_ts)-168)


acf(well_station, lag=40)
pacf(well_station, lag=40)

auto = auto.arima(well_station, seasonal = FALSE, xreg=fourier(well_station,K=7))
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
pacf(auto$residuals, lag=60)


well_arima = Arima(well_station, order=c(2, 0, 2), method="ML")
summary(well_arima)
acf(well_arima$residuals, lag=60)
pacf(well_arima$residuals, lag=60)

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(well_arima$residuals, lag = i, type = "Ljung", fitdf = 1)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

arima.trig<-Arima(well_station,order=c(2,0,2), xreg=fourier(well_station,K=50))   #fourier is a combination of fitting sines and cosines
# K= _ means how many sine/cosine terms you want
 summary(arima.trig)
acf(arima.trig$residuals, lag=60)
pacf(arima.trig$residuals, lag=60)

plot(arima.trig$residuals)s

           