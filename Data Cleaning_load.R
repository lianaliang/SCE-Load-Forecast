library(forecast)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)

############# 1. clean data for missing value of load

df = read_excel("20140101-20190901 SCE & CAISO Actual Load  9 27 2019.xlsx", sheet = "SCE Load Only", col_names
                = T)  # 49342 records
df$Date = as.POSIXct(as.character(df$Date), format = "%Y-%m-%d %H:%M")
df = df %>%
  group_by(Date) %>%
  summarise(load = mean(load)) ## 49337 records

# calculate the average of each day of year
df$mondayhour = paste(month(df$Date), mday(df$Date), hour(df$Date))
average = df %>%
  group_by(mondayhour) %>%
  summarise(average = mean(load))

# build a dataframe with the completed time sequence 
all = data.frame(Date = seq(as.POSIXct("2014-01-01 00:00:00") , as.POSIXct("2019-09-01 23:00:00"), by = 3600))
all = left_join(all,df, by = "Date")
all$mondayhour = paste(month(all$Date), mday(all$Date), hour(all$Date))
all = left_join(all, average, by = "mondayhour")
all$load[is.na(all$load)] <- all$average[is.na(all$load)]
all[which(is.na(all$load)),"load"] = average[average$mondayhour == "2 29 13", "average"]

# check no na value left:
sum(is.na(all$load))

df = all[,c("Date","load")]

temp=read.csv('temp.csv')[,-1]
df2=df%>%
  group_by(Date)%>%
  filter(row_number()==1 | row_number()==n())

df2[df2$Date=='2014-11-02 01:00:00',]
