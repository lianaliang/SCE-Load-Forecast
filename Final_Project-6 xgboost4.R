library(forecast)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(chron)
library(timeDate)
library(dummies)
library(data.table)
library(leaps)
library(glmnet)
library(xgboost)


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


# build a dataframe with the complete time sequence 
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

df = all[,c("Date","load","mondayhour")]


##############################################################################

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

#plot(df$temperature[1:(24*60)], type = 'l')

########################################## 2.2 read daily temperature data (not used in below's model)
temp_d = read_excel("SCETemps14-19.xlsx", col_names = T)

temp_d$Date = as.Date(temp_d$Date)
colnames(temp_d) = gsub(" ", "", colnames(temp_d))
colnames(temp_d) = gsub("-", "_", colnames(temp_d))

df$date_r = as.Date(df$Date)
df = left_join(df, temp_d, by = c("date_r" = "Date"))

##############################################################################



########################## 3. Build variables

######## 3.1 create the previous 3 years average value

average2 =  df[1:((365*3+1)*24),]%>%
  group_by(mondayhour) %>%
  summarise(avg = mean(load))
df = df %>%
  left_join(average2, by = "mondayhour")

######## 3.2 create lag for two days before, three days before, and one week before, since lag 48, 72 is significant, and weekly patern is obvious
n = nrow(df)
l0 = c(df$load)
df$l2 = c(rep(0,48), l0)[1:n]
df$l3 = c(rep(0,72), l0)[1:n]
df$l4 = c(rep(0,24*4), l0)[1:n]
df$l5 = c(rep(0,24*5), l0)[1:n]
df$l6 = c(rep(0,24*6), l0)[1:n]
df$l7 = c(rep(0,24*7), l0)[1:n]
df$l14 = c(rep(0,24*14), l0)[1:n]
df$l21 = c(rep(0,24*21), l0)[1:n]
df$w4 = c(rep(0,24*7*4), l0)[1:n]
df$w8 = c(rep(0,24*7*8), l0)[1:n]
df$w12 = c(rep(0,24*7*12), l0)[1:n]
df$w16 = c(rep(0,24*7*16), l0)[1:n]
df$w20 = c(rep(0,24*7*20), l0)[1:n]
df$w24 = c(rep(0,24*7*24), l0)[1:n]

df$l2_2 = (df$l2)^2
df$l2_3 = (df$l2)^3

######## 3.3 create hour of day, day of week, day of month, etc..

hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay",
           "USNewYearsDay","USThanksgivingDay")        
myholidays  <- dates(as.character(holiday(2014:2019,hlist)),format="Y-M-D")

df$holiday = as.integer(is.holiday(df$Date,myholidays))
df$hour = hour(df$Date)
df$hour_2 = (df$hour)^2
df$hour_3 = (df$hour)^3
df$hour_sin = sin(2*pi*((df$hour+12)%%24)/24)
df$hour_cos = cos(2*pi*((df$hour+12)%%24)/24)
df$day_week = wday(df$Date)
d = data.frame(dummy(df$day_week))
df = cbind(df,d)

df$day_week_2 = (df$day_week)^2
df$day_week_sin = sin(2*pi*df$day_week/7)
df$day_week_cos = cos(2*pi*df$day_week/7)
df$wday = ifelse(df$day_week<=5,1,0)
df$month = month(df$Date)
df$day_month = mday(df$Date)
df$day_year = yday(df$Date)
df$day_year_2 = (df$day_year)^2
df$day_year_3 = (df$day_year)^3
df$day_year_sin = sin(2*pi*df$day_year/365.25)
df$day_year_cos = cos(2*pi*df$day_year/365.25)
df$hour_day_year_2 = df$hour * (df$day_year)^2
df$hour_day_year = df$hour * df$day_year
df$quarter = quarter(df$Date)
df$year = year(df$Date)
df$warm = ifelse(df$month %in% c(5,6,7,8,9), 1, 0)
df$warm_hour = df$warm * df$hour
df$warm_hour_2 = df$warm * (df$hour)^2
df$trend = seq(1,n,1)
df$cca = ifelse(as.Date(df$Date) > as.Date("2019-03-01"), 1,0)

df$LAX_High_2 = (df$LAX_High)^2
df$lax_low_2 = (df$lax_low)^2
df$RIV_High_2 = (df$RIV_High)^2
df$riv_low_2 = (df$riv_low)^2

rm(d, all, average, average2, temp, temp_d)



##############################################################################

########################## 4. Build the model

## use the past 90 days' data to build the model, 3 month's data prediction is better than one year data, one year is two much
start_2017 = (365+365+366)*24+1
start_2018 = (365+365+366+365)*24 +1
start_2019 = (365+365+366+365+365)*24 +1

ntrain = 24*2*365 +8
nvalid = 40
len = ntrain+nvalid


remove  = c("day_week","temperature","avg","mondayhour","date_r","CQT_High","cqt_low","WJF_High","wjf_Low","Date" , "load","zone" )
variables = colnames(df)[! colnames(df) %in% remove]

result = data.frame(Date=as.Date(character()), MAPE=double(), peak= integer(), peak_pred = integer(), stringsAsFactors=FALSE) 

i = 0  ## 0 is Jan-1, 24 is Jan-2, etc

## for year 2017:

for( i in seq(24*0,24*364,24)){

  initial.df = df[(start_2017+24-len+i):(start_2017-len+ntrain-1+i+24),]
  train.df = initial.df[1:(24*729),]
  valid.df = initial.df[(24*729+1):ntrain,]
  test.df = df[(start_2017+24-len+ntrain+i):(start_2017+24-1+i),]
  
  ## Use one week data as the validation set
  predictors_train  = as.matrix(train.df[,variables])
  response_train = train.df$load
  train_m = xgb.DMatrix(predictors_train,label=response_train)
  
  predictors_valid  = as.matrix(valid.df[,variables])
  response_valid = valid.df$load
  valid_m = xgb.DMatrix(predictors_valid,label=response_valid)
  
  predictors_test  = as.matrix(test.df[,variables])
  response_test = test.df$load
  test_m = xgb.DMatrix(predictors_test,label=response_test)
  
  params = list(booster= 'gbtree', eta=0.1, max_depth=5, subsample = 0.9,
                gamma=0.6,colsample_bytree = 1,colsample_bylevel = 1, lambda = 0.1,
                min_child_weight = 2,objective = 'reg:linear',eval_metric = 'rmse')
  
  watchlist = list(train = train_m, eval = valid_m)
  
  micro_model = xgb.train(data = train_m, params =  params, watchlist = watchlist,nrounds=120, early_stopping_rounds = 10,verbose = 0)
  
  predicts = predict(micro_model,test_m, type = 'response')
  accurate = data.frame( accuracy(response_test[17:40], predicts[17:40]))
  p = predicts[17:40]
  t = response_test[17:40]
  p_pred = paste(which(p == max(p)) -1, collapse=" ")
  p_t = paste(which(t == max(t)) -1,collapse=" ")

result[(i/24+1),"MAPE"] = accurate$MAPE
result[(i/24+1), "Date"] = as.Date("2017-01-01")+i/24
result[(i/24+1), "peak"] = p_t
result[(i/24+1), "peak_pred"] = p_pred
print(i/24)

}


xgb.importance(colnames(train_m), model = micro_model)

ggplot(data = result, aes(x = Date, y = MAPE))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("MAPE in 2017")+
  theme_bw()

mean(result$MAPE, na.rm = T)  # first three month is 3.294321


write.csv(result, 'MAPE-2.csv')

