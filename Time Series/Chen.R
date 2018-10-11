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

# ----------------------- plotting ---------------------------------
# stl decomposition using original data
decomp_stl <- stl(train.ts, s.window = 7) 
decomp_stl$time.series
fourier.well = tslm(train.ts ~ fourier(train.ts,K=7))
stlresult <- data.frame(date=time(decomp_stl$time.series),decomp_stl$time.series)
stlresult$seas_fit = fourier.well$fitted.values - 3.742628

# Figure 3: Well Water Depth Seasonality Overlaid with Fitted Model
ggplot(stlresult[1:93623,],aes(x=date,y=seasonal,colour="Seasonal Pattern")) +
  geom_line() +
  geom_line(aes(x=date,y=seas_fit,colour="Fitted Model")) +
  ggtitle("Figure 3: Well Water Depth Seasonality Overlaid with Fitted Model") +
  ylab("Well Depth (Ft)") +
  xlab("Year") +
  #scale_colour_manual(name="Time Series Label",
  #                   values=c(Seasonal_Pattern="black", Fitted_Model="red")) +
  theme(
    legend.box.background = element_rect(fill = "aliceblue",colour = "aliceblue",
                                         size = 0.5, linetype = "solid"),
    legend.position="right", 
    panel.background = element_rect(fill = NULL,colour = NULL,
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"),
    plot.title = element_text(hjust = 0.5)
  )


# Figure 2: Well Water Depth Overlaid with Trend
ggplot(stlresult[1:93623,],aes(x=date,y=seasonal + trend+remainder, colour="Water Depth")) +
  geom_line() +
  geom_line(aes(x=date,y=trend,colour="Trend")) +
  ggtitle("Figure 2: Well Water Depth Overlaid with Trend") +
  ylab("Well Depth (Ft)") +
  xlab("Year") + 
  #scale_colour_manual(name="Time Series Label",
  #values=c(Water_Depth="black", Trend="red")) +
  theme(
    legend.box.background = element_rect(fill = "aliceblue",colour = "aliceblue",
                                         size = 0.5, linetype = "solid"),
    legend.position="right", 
    plot.title = element_text(hjust = 0.5),
    panel.background = element_rect(fill = "aliceblue",colour = "aliceblue",
                                    size = 0.5, linetype = "solid"),
    panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                    colour = "white"),
  )

# Figure 1: Predicted / Acutal values comparison
arima.1<-Arima(train.ts,order=c(12,1,2),xreg=fourier(train.ts,K=7))

pred = forecast(arima.1, xreg=fourier(train.ts, K=7,h=168),h=168)
error.arima1 <- test.ts-pred$mean
MAE=mean(abs(error.arima1))
MAE
MAPE=mean(abs(error.arima1)/abs(test.ts))
MAPE
autoplot(pred$mean)
test.time = well$new_date_time[93624:93791]

plot_df = data.frame(forecast=pred$mean, time=test.time,actual=test.ts)

ggplot(plot_df, aes(as.POSIXct(time))) +
  geom_line(aes(y =actual, colour = "Observed")) +
  geom_line(aes(y =forecast, colour = "Predicted")) +
  ggtitle("Figure 1: Predicted VS Actual Well Depth")+
  xlab("Time")+ylab("Well depth (Ft)")+
  ylim(6,7) + 
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = c(0.9, 0.8),
        panel.background = element_rect(fill = "aliceblue",colour = "aliceblue",
                                        size = 0.5, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "white")
  )

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
