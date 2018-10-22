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
f179 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\F-179.xlsx', sheet = "Well")
f319 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\F-319.xlsx')
g561 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-561_T.xlsx')
g580 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-580A.xlsx')
g852 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-852.xlsx')
g860 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-860.xlsx')
g1220 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-1220_T.xlsx')
g2147 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-2147_T.xlsx')
g2866 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-2866_T.xlsx')
g3549 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\G-3549.xlsx')
pb1680 = read_excel('C:\\Users\\thebi\\OneDrive\\Documents\\Time Series\\Well_Data\\Well Data\\PB-1680_T.xlsx')

f45$date
f45$time <- str_replace(f45$time, "1899-12-31 ","") # replace the time column to get rid of ymd.
f45$date_new <- substr(f45$date,1,10) # aggregated level
f45
class(f45$date)

df <- sqldf("select date_new,
            avg(Corrected) as well_ft from f45 
            group by date_new")
View(df)
dim(df)

# ------------------ generating a continuous sequence of times ------------
date_check <- c()
for (i in seq.Date(as.Date("2007-10-01"),as.Date("2018-06-12"),"month")){
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
df_continuous <- data.frame(date_new=date_check[-1])
df_continuous <-left_join(df_continuous,df,by="date_new")
View(df_continuous)

# check the dates with missing values
missing_date = subset(df_continuous, is.na(predicted_tide))
missing_date
df_continuous <- df_continuous %>%
  mutate(date_hour = date_agg,date_agg =NULL)
summary(df_continuous)








