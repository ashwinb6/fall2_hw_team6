library(data.table)
library(haven)
library(readxl)
library(stringr)
library(sqldf)
library(lubridate)
library(dplyr)
library(imputeTS)

f45 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\F-45.xlsx', sheet="Well")
colnames(f45)
f319 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\F-319.xlsx', sheet="Well")
g561 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-561_T.xlsx', sheet="Well")
g580 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-580A.xlsx', sheet="Well")
g852 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-852.xlsx', sheet="Well")
g860 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-860.xlsx', sheet="Well")

f45$date
f45$time <- str_replace(f45$time, "1899-12-31 ","") # replace the time column to get rid of ymd.
f45$date_new <- paste(f45$date,f45$time,sep="-")
f45$date_new <- substr(f45$date_new,1,13) # aggregated level
f45

f319$time <- str_replace(f319$time, "1899-12-31 ","") # replace the time column to get rid of ymd.
f319$date_new <- paste(f319$date,f319$time,sep="-")
f319$date_new<- substr(f319$date_new,1,13)

colnames(g580)
g580$time <- str_replace(g580$time, "1899-12-31 ","") # replace the time column to get rid of ymd.
g580$date_new <- paste(g580$date,g580$time,sep="-")
g580$date_new<- substr(g580$date_new,1,13)

df2 <- sqldf("select date_new,
            avg(Corrected) as well_ft from g580
            where Code = 'A'
            group by date_new")

View(df2)
dim(df)
dim(df2)

# ------------------ generating a continuous sequence of times ------------
date_check <- c()
for (i in seq.Date(as.Date("2007-10-01"),as.Date("2018-04-09"),"days")){
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
df_continuous <- data.frame(date_new=date_check[-11])
df_continuous <-left_join(df_continuous,df2,by="date_new")
View(df_continuous)
dim(df_continuous)
# check the dates with missing values
missing_date = subset(df_continuous, is.na(well_ft))
count(missing_date)
df_continuous <- df_continuous %>%
  mutate(date_hour = NULL)
summary(df_continuous)
df_continuous = head(df_continuous, -11)

df_continuous = na.kalman(df_continuous)
View(df_continuous)
fwrite(df_continuous, "G-580.csv")
