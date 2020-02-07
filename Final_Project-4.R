library(forecast)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)


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


########################## 2.1 read hourly temperature data

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

#plot(df$temperature[1:(24*60)], type = 'l')

########################## 2.2 read daily temperature data
temp_d = read_excel("SCETemps14-19.xlsx", col_names = T)

temp_d$Date = as.Date(temp_d$Date)
colnames(temp_d) = gsub(" ", "", colnames(temp_d))
colnames(temp_d) = gsub("-", "_", colnames(temp_d))

df$date_r = as.Date(df$Date)
df = left_join(df, temp_d, by = c("date_r" = "Date"))


########################## 3. Build the model
## create hour of day, day of week, day of month, etc..
n = nrow(df)
df$hour = hour(df$Date)
df$day_week = wday(df$Date)
df$wday = ifelse(df$day_week<=5,1,0)
df$day_month = mday(df$Date)
df$month = month(df$Date)
df$day_year = yday(df$Date)
df$quarter = quarter(df$Date)
df$trend = seq(1,n,1)
df$cca = ifelse(as.Date(df$Date) > as.Date("2019-03-01"), 1,0)
df$twelve = as.integer(ifelse(df$hour %% 24 == 12, 1,0))
df$twenty = as.integer(ifelse(df$hour %% 20 == 0, 1,0))
df$eleven = as.integer(ifelse(df$hour %% 22 == 11, 1,0))
df$ten = as.integer(ifelse(df$hour %% 20 == 10, 1,0))
df$nineteen = as.integer(ifelse(df$hour %% 19 == 0, 1,0))

## use the past 90 days' data to build the model, 3 month's data prediction is better than one year data, one year is two much
start_2017 = (365+365+366)*24+1
start_2018 = (365+365+366+365)*24 +1
start_2019 = (365+365+366+365+365)*24 +1

ntrain = 24*90+8
nvalid = 40
len = ntrain+nvalid


## for year 2017:
result = data.frame(Date=as.Date(character()), MAPE=double(), stringsAsFactors=FALSE) 

i = 0
# For loop
for( i in seq(24*0,24*30,24)){

y= ts(df$load[(start_2017-len+i):(start_2017-1+i)])
train.ts = window(y, end = c(1, ntrain))
valid.ts = window(y, start = c(1,ntrain+1))

# remove  = c("Date" , "load","zone" )
# variables = colnames(df)[! colnames(df) %in% remove]
# paste(variables, collapse = "+")

train.df = df[(start_2017-len+i):(start_2017-len+ntrain-1+i),]
valid.df = df[(start_2017-len+ntrain+i):(start_2017-1+i),]

formula = as.formula("train.ts~nineteen +wday+I(hour*day_week)+poly(hour,3)+ I(sin(pi*((hour+7) %% 24)/24))+I(cos(pi*((hour+12)%%24)/24))+I(sin(2*pi*day_week/7))+I(cos(2*pi*day_week/7))+day_week+I(day_week^2)+ I(sin(2*pi*day_month/30))+I(cos(2*pi*day_month/30))+day_month+I(day_month^2)  +I(sin(2*pi*day_year/365.25))+I(cos(2*pi*day_year/365.25))+cca")
lm_model = tslm(formula, data = train.df)
pred_lm = forecast(lm_model, newdata = valid.df,h = nvalid, level = 0)
# accuracy(pred_lm$mean[17:40], valid.ts[17:40]) ## MAPE for linear regression is now  3.324325 for 2017-Jan-01

# summary(lm_model)

# plot(train.df$load[1:120], type = "l")

# par(mfrow= c(1,1))
# plot(pred_lm)
# lines(valid.ts, col = 'red')
# tsdisplay(pred_lm$residuals)


#### Second layer, Add arima can increase the accuracy
resid = pred_lm$residuals

resid_ts = ts(resid,frequency = 24)

# ndiffs(resid_ts)  
# ndiffs(diff(resid_ts, lag = 1))
# nsdiffs(resid_ts)
# nsdiffs(diff(resid_ts, lag = 24))

res_arima = Arima(resid_ts, order = c(1,0,1), seasonal = c(1,0,1)) # need some time
arima_pred = forecast(res_arima, h = nvalid)

# summary(res_arima)
# tsdisplay(res_arima$residuals)  ## don't know why there still have some significant lags
final_pred = pred_lm$mean + ts(arima_pred$mean,start = c(1,ntrain+1))
accurate = data.frame(accuracy(final_pred[17:40], valid.ts[17:40])) 
# accurate$MAPE

result[(i/24+1),"MAPE"] = accurate$MAPE
result[(i/24+1), "Date"] = as.Date("2017-01-01")+i/24
ntrain = ntrain +24
len = ntrain +nvalid
}

ggplot(data = result, aes(x = Date, y = MAPE))+
  geom_line()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("MAPE in 2017 Jan")+
  theme_bw()

mean(result$MAPE, na.rm = T)  ## 4.104047 for 2017-Jan
  