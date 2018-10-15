library(haven)
library(readxl)
library(stringr)
library(sqldf)
library(lubridate)
library(dplyr)
library(imputeTS)



read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\station_8722802.csv") # by default it will read in the first excel sheet "Rain"
data <- read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\station_8722802.csv", header = TRUE) 
dim(data)
View(data)
#---------- aggregate the data to hourly level ---------------
data$date_new <- paste(data$Date,data$Time,sep="-") # make a new column to combine date and time
data$date_agg <- substr(data$date_new,1,13) # aggregated level
class(data$date)

df <- sqldf("select date_agg,
            avg(Prediction) as predicted_tide from data 
            group by date_agg")
View(df)
dim(df)

# ------------------ generating a continuous sequence of times ------------
date_check <- c()
for (i in seq.Date(as.Date("2007-10-01"),as.Date("2018-06-12"),"days")){
  #print(i)
  i<- as.Date(i,origin = "1970-01-01")
  #i <- strptime(i, format="%Y-%m-%d")
  #print(i)
  for (j in seq(0,23)){
    jj <- str_pad(as.character(j),2,side="left",pad="0")
    ele <- paste(i,jj,sep="-")
    date_check <- c(date_check,ele)
  }
}
date_check[-1] 


# -------------------- make a continuous time series including missing hours ----------------
df_continuous <- data.frame(date_agg=date_check[-1])
df_continuous <-left_join(df_continuous,df,by="date_agg")
View(df_continuous)

# check the dates with missing values
missing_date = subset(df_continuous, is.na(predicted_tide))
missing_date
df_continuous <- df_continuous %>%
  mutate(date_hour = date_agg,date_agg =NULL)
summary(df_continuous)

# ------------------ save out data to csv file ------------------
write.csv(df_continuous,file="hourly_tide.csv")


# Aggregating Well and Tide Data ------------------------------------------
final_data <- na.kalman(df_continuous)
write.csv(final_data, file="final_tide.csv", row.names = F)
summary(final_data)

well_data = read.csv("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\well_agg_with_impute.csv", header = T)
well_and_tide = cbind(final_data,well_data)
well_tide_clean = sqldf("select new_date_time, Corrected_w_imputations as Well_ft, predicted_tide
                        from well_and_tide")
View(well_tide_clean)


# Aggregate Rain Data -----------------------------------------------------
dataRain <- read_excel("C:\\Users\\thebi\\OneDrive\\Documents\\GitHub\\fall2_hw_team6\\Time Series\\G-1260_T.xlsx", sheet = "Rain") 
dim(dataRain)
View(dataRain)
#---------- aggregate the data to hourly level ---------------
dataRain$date_agg <- substr(dataRain$Date,1,13) # aggregated level
class(data$Date)

dfRain <- sqldf("select date_agg,
            sum(RAIN_FT) as rain_ft from dataRain 
            group by date_agg")
View(dfRain)
dim(dfRain)

# ------------------ generating a continuous sequence of times ------------
date_check <- c()
for (i in seq.Date(as.Date("2007-10-01"),as.Date("2018-06-12"),"days")){
  #print(i)
  i<- as.Date(i,origin = "1970-01-01")
  #i <- strptime(i, format="%Y-%m-%d")
  #print(i)
  for (j in seq(0,23)){
    jj <- str_pad(as.character(j),2,side="left",pad="0")
    ele <- paste(i,jj,sep=" ")
    date_check <- c(date_check,ele)
  }
}
date_check[-1] 


# -------------------- make a continuous time series including missing hours ----------------
df_continuous <- data.frame(date_agg=date_check[-1])
df_continuous <-left_join(df_continuous,df,by="date_agg")
View(df_continuous)

# check the dates with missing values
missing_date = subset(df_continuous, is.na(dfRain$rain_ft))
missing_date
df_continuous <- df_continuous %>%
  mutate(date_hour = date_agg,date_agg =NULL)
summary(df_continuous)

# ------------------ save out data to csv file ------------------
write.csv(dfRain,file="hourly_rain.csv", row.names = F)

