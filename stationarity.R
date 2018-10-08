library(haven)
library(forecast)
library(fma)
library(tseries)
library(expsmooth)
library(lmtest)
library(zoo)
library(ggplot2)
library(imputeTS)
library(lubridate)
library(seasonal)


well_data <- read.csv("../time_series2/hw1/data/well_agg_with_impute.csv")

#---------------------------------------------------------------------------
# exploring different season 
#---------------------------------------------------------------------------

# creating time series object with 24 as length of season

ts_well_hourly <- ts(well_data$Corrected_w_imputations, frequency = 24)
ts_well_hourly_log <- log(ts_well_hourly)

autoplot(ts_well_hourly)
autoplot(ts_well_hourly_log)

# decomposing the time series

well_stl <- stl(x = ts_well_hourly, s.window = 12)

autoplot(well_stl)

# decomposing the time series - multiplicative

well_stl_log <- stl(x = ts_well_hourly_log, s.window = 12)

autoplot(well_stl_log)

# creating time series object with 24 * 365.25 as length of season

ts_well_yearly <- ts(well_data$Corrected_w_imputations, frequency = 24 * 365.25)
ts_well_yearly_log <- log(ts_well_yearly)

autoplot(ts_well_yearly)
autoplot(ts_well_yearly_log)

# decomposing the time series

well_stl_yearly <- stl(x = ts_well_yearly, s.window = 12)

autoplot(well_stl_yearly)

# decomposing the time series - multiplicative

well_stl_yearly_log <- stl(x = ts_well_yearly_log, s.window = 12)

autoplot(well_stl_yearly_log)

# trying out multiple seasons - hourly and monthly

ts_hour_month <- msts(well_data$Corrected_w_imputations,
                      seasonal.periods = c(24, 24 * 30))

ts_hour_month %>% mstl() %>%
  autoplot()

# multiplicative
ts_hour_month_log <- msts(log(well_data$Corrected_w_imputations),
                      seasonal.periods = c(24, 24 * 30))

ts_hour_month_log %>% mstl() %>%
  autoplot()


# trying out multiple seasons - hourly and yearly

ts_hour_year <- msts(well_data$Corrected_w_imputations,
                      seasonal.periods = c(24, 24 * 365.25))

ts_hour_year %>% mstl() %>%
  autoplot()

# multiplicative
ts_hour_year_log <- msts(log(well_data$Corrected_w_imputations),
                          seasonal.periods = c(24, 24 * 365.25))

ts_hour_year_log %>% mstl() %>%
  autoplot()

# trying out multiple seasons - monthly and yearly

ts_month_year <- msts(well_data$Corrected_w_imputations,
                     seasonal.periods = c(24 * 30, 24 * 365.25))

ts_month_year %>% mstl() %>%
  autoplot()

# multiplicative
ts_month_year_log <- msts(log(well_data$Corrected_w_imputations),
                          seasonal.periods = c(24 * 30, 24 * 365.25))

ts_month_year_log %>% mstl() %>%
  autoplot()

#---------------------------------------------------------------------------
# testing for stationarity
#---------------------------------------------------------------------------

diff_ts <- diff(ts_well_yearly)

autoplot(diff_ts)

# stationarity test on the original data
adf.test(ts_well_yearly, alternative = "stationary", k = 0)

# stationarity test on lag 1 date
adf.test(diff_ts, alternative = "stationary", k = 0)




















