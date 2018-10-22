library(forecast)
library(haven)
library(fma)
library(expsmooth)
library(lmtest)
library(zoo)
library(seasonal)
library(tseries)
library(sqldf)
library(data.table)

head(well_data)
well_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\well_agg_with_impute.csv")
rain_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\FinalProject\\hourly_rain2.csv")
tide_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\FinalProject\\final_tide.csv")
combine=cbind(well_data,rain_data,tide_data)
head(all_data)
all_data = sqldf("select new_date_time as date, Corrected_w_imputations as well_ft, predicted_tide as tide, rain_ft as rain
                 from combine")
all_data = data.table(all_data)
nrow(all_data)
length(all_data)

fwrite(all_data, "Well_G1260.csv")

train=subset(all_data, end=length(all_data)-169)
length(train)

well_ts = ts(all_data$well_ft, start = c(2007,10), frequency = 24*365.25)

length(well_ts)
length(rain)


# Model 1 Attempt ---------------------------------------------------------

model1=Arima(well_ts,order=c(5,0,8),xreg=x.reg, method="ML") #MAPE .163
summary(model1)
ndiffs(model1$residuals)

White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(auto1$residuals, lag = i, type = "Ljung", fitdf = 3)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags", ylim = c(0, 0.2))
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")

Acf(model1$residuals, lag=100)
Pacf(model1$residuals, lag=100)


# Auto ARIMA ---------------------------------------------------------------

auto1 = auto.arima(well_ts,xreg=x.reg)  #(2,1,1) MAPE .163
Acf(auto1$residuals, lag=100)
Pacf(auto1$residuals, lag=100)

plot(auto1$residuals)


# Model 2 Attempt ---------------------------------------------------------

model2=Arima(well_ts,order=c(24,0,8),xreg=x.reg, method="ML") #MAPE 
