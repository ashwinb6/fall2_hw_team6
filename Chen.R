library(haven)
library(tseries)
library(fma)
library(forecast)
library(ggplot2)

setwd("/Users/chenhuizhang/Desktop/Fall 2/Time series 2/HW")
well = read.csv("well_agg_with_impute.csv")
View(well)
dim(well)
summary(well)


well.ts<-ts(well$Corrected_w_imputations,frequency = 365.25*24)
train.ts=subset(well.ts,end=length(well.ts)-168)
test.ts=subset(well.ts,start=length(well.ts)-167)

length(train.ts)
length(test.ts)

# Model Selection based on AIC / AICC / BIC 
# -------------------------- AUTO SELECTION -----------------------------
# auto ARIMA selection : ARIMA(2,1,2) with drift
auto.best= auto.arima(train.ts)

Acf(auto.best$residuals, main = "")$acf
Pacf(auto.best$residuals, main = "")$acf

# auto ARIMA exhuasive selection: ARIMA(1,1,3)
auto.arima(train.ts, stepwise=FALSE, approximation=FALSE)
auto.best2 <-Arima(train.ts,order=c(1,1,3))
summary(auto.best2)
Acf(auto.best2$residuals, main = "")$acf
Pacf(auto.best2$residuals, main = "")$acf
autoplot(forecast(auto.best2),h=24*7)
# auto ARIMA with sin and cosine terms:ARIMA(2,1,2) --- BEST
fit <- auto.arima(train.ts, seasonal=FALSE,xreg=fourier(train.ts, K=7))
summary(fit)
Acf(fit$residuals, main = "")$acf
Pacf(fit$residuals, main = "")$acf
checkresiduals(fit) # check the residual plot, distribution, ACF 

autoplot(forecast(fit, xreg=fourier(train.ts, K=7)),h=24*7)

# White noist LB test
White.LB <- rep(NA, 10)
for(i in 1:10){
  White.LB[i] <- Box.test(fit$residuals, lag = i, type = "Ljung", fitdf = 2)$p.value
}

White.LB <- pmin(White.LB, 0.2)
barplot(White.LB, main = "Ljung-Box Test P-values", ylab = "Probabilities", xlab = "Lags")
abline(h = 0.01, lty = "dashed", col = "black")
abline(h = 0.05, lty = "dashed", col = "black")
