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


well_data <- read.csv("../data/well_agg_all_times.csv")

start_date <- ymd(substr(as.character(well_data$new_date_time[1]),1,10), tz = Sys.timezone())
end_date <- ymd(substr(as.character(well_data$new_date_time[nrow(well_data)]),1,10), 
                    tz = Sys.timezone())

ts_well <- ts(data = well_data$Corrected, frequency = 24)
ts_well_imputed <- na.seadec(x = ts_well, algorithm = "ma")
ts_well_imputed_as_vector <- as.vector(ts_well_imputed)

well_data$Corrected_w_imputations <- ts_well_imputed_as_vector
well_data$imputed <- well_data$Corrected != well_data$Corrected_w_imputations

write.csv(well_data, file = "./MSA_2019/course_work/fall/fall_2/time_series2/hw1/data/well_agg_with_impute.csv")



