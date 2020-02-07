library(forecast)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)


########################################  1. clean data for missing value of load
df = read_excel("20140101-20190901 SCE & CAISO Actual Load  9 27 2019.xlsx", sheet = "SCE Load Only", col_names
                = T)  # 49342 records
df$Date = as.POSIXct(as.character(df$Date), format = "%Y-%m-%d %H:%M",tz = "UTC")
df = df %>%
  group_by(Date) %>%
  summarise(load = mean(load)) ## 49337 records  ## 5 duplicate values?

# calculate the average of each day of year
df$mondayhour = paste(month(df$Date), mday(df$Date), hour(df$Date)) ## hour: 0-23
average = df %>%
  group_by(mondayhour) %>%
  summarise(average = mean(load)) ## 366*24 = 8784 != 8783


# build a dataframe with the completed time sequence 
all = data.frame(Date = seq(as.POSIXct("2014-01-01 00:00:00",tz = "UTC") , as.POSIXct("2019-09-01 23:00:00",tz = "UTC"), by = "hour"))
all$yearmondayhour = paste(year(all$Date),month(all$Date), mday(all$Date), hour(all$Date))
all = all %>%
  group_by(yearmondayhour) %>%
  summarise(Date=min(Date))
all <- all[,'Date']
all = left_join(all,df, by = "Date")

#sum(is.na(all$load)) ##NA:342
all$mondayhour = paste(month(all$Date), mday(all$Date), hour(all$Date))
all = left_join(all, average, by = "mondayhour")
all$load[is.na(all$load)] <- all$average[is.na(all$load)]

#all[which(is.na(all$load)),"Date"] ##"2016-02-29 14:00:00 PST"
all[which(is.na(all$load)),"load"] = average[average$mondayhour == "2 29 13", "average"]

# check no na value left:
sum(is.na(all$load))

df = all[,c("Date","load")]


########################################## 2.1 read hourly temperature data

temp = read.csv("temp.csv")
temp$date = as.POSIXct(temp$date,format = "%Y-%m-%d %H:%M",tz = "UTC")
temp = temp %>%
  group_by(date) %>%
  summarise(temperature = mean(Means))
#temp$date = as.POSIXct(as.character(temp$date), tryFormats = c("%m/%d/%Y %H:%M"))
#temp$date = as.character(temp$date)
#substring(temp$date,1,2) <- "20"

df = left_join(df, temp, by = c("Date"= "date"))
df = df %>%
  arrange(Date)

sum(is.na(df))
#df[which(is.na(df$temperature)),"Date"]

plot(df$temperature[1:(24*60)], type = 'l')

write.csv(df,'data_filled.csv')





