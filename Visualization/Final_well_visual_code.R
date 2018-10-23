library(haven)
library(readxl)
library(stringr)
library(sqldf)
library(lubridate)
library(dplyr)
library(imputeTS)
library(tseries)
library(forecast)
library(ggplot2)

#load well tide rain data into three seperate R tables
well_n=read_excel("/Users/peixingli/Desktop/TimeSeries_MSA/WellVisual/PB-1680_T.xlsx",sheet =3)
#Get the hour information
well_n$time<- as.POSIXct(strptime(well_n$time,format="%Y-%m-%d %H:%M:%S"))
well_n$hour<-hour(well_n$time)
#Agggregate hour data
well<-well_n %>%
  group_by(date,hour)%>%
  mutate(well_hour=mean(Corrected))%>%
  filter(Code=="A")
#Remove duplicate so the data frame only has hourly data
well<-distinct(well,date,hour,.keep_all = TRUE)
# ------------------ generating a continuous sequence of times ------------

#Create time series of hourly data 
time<-as.data.frame(seq(as.POSIXct("2007-10-10 01:00:00"), as.POSIXct("2018-06-07 11:00:00"), by="hour"))
colnames(time)<-("time")



# ------------------ left join complete time sereis and well data ------------
well$time <- as.POSIXct(paste(well$date,well$hour), format='%Y-%m-%d %H')

#Join 
well_join <- left_join(time,well,by="time" )
#There are 17581 missing values
sum(is.na(well_join$Corrected))

well_complete<-well_join%>%
  select(time,well_hour)

decomp_stl <- stl(well_time_series, s.window = 7)

#-------------------Impute data with locf -----------

well_impute<-well_complete%>%mutate(well_impute=na.locf(well_complete$well_hour))
#check and see there is o missing values afrer imputing
sum(is.na(well_impute$well_impute))


#-------------------Build Arima Model----------------
#create time sereis object of the well data
well_time_series=ts(well_impute$well_impute,start=c(2007,10),frequency = 24*365.25)
train=subset(well_time_series,end=length(well_time_series)-168)
test=subset(well_time_series,start=length(well_time_series)-167)
##build model
ndiffs(train)
model=Arima(train,order=c(2,1,2))
summary(model)
auto.arima(train,max.p=100,max.q=100,seasonal=FALSE)   
checkresiduals(model)
forecast=forecast(model, h=168)
accuracy(forecast,test)
#--------------------create a predicted plot form the dataset-------------------------

time_plot=well_impute$time[((length(well_impute$time))-167):(length(well_impute$time))]
fit=forecast$mean
plot_df=data.frame(as.POSIXct(time_plot),as.matrix(fit),as.matrix(test))
names(plot_df)<-c('time','forecast','observe')
#------------------Left join test data set--------------------------
well_final <- left_join(well_impute,plot_df,by="time" )%>%select(time,well_hour,well_impute,forecast)
#------------------Output to CSV---------------------------------
write.csv(well_final, file = "/Users/peixingli/Desktop/TimeSeries_MSA/Visual/FinalPB-1680_T.csv")




