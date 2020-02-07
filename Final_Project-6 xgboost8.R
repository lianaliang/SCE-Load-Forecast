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

setwd("~/2019 fall/Time Series/final project")
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

df$y1 = c(rep(0,24*365), l0)[1:n]
df$y1_1a = c(rep(0,24*366), l0)[1:n]
df$y1_1b = c(rep(0,24*364), l0)[1:n]
df$y1_1c = c(rep(0,24*363), l0)[1:n]

df$y2 = c(rep(0,24*(365+365)), l0)[1:n]
df$y2_1b = c(rep(0,24*(365+364)), l0)[1:n]


df$l2_2 = (df$l2)^2
df$l2_3 = (df$l2)^3
df$l3_2 = (df$l3)^2
df$l4_2 = (df$l4)^2
df$l7_2 = (df$l7)^2

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
df$hour_sin1 = sin(2*pi*1*df$hour/24)
df$hour_sin2 = sin(2*pi*2*df$hour/24)
df$hour_sin3 = sin(2*pi*3*df$hour/24)
df$hour_sin4 = sin(2*pi*4*df$hour/24)
df$hour_sin5 = sin(2*pi*5*df$hour/24)
df$hour_sin6 = sin(2*pi*6*df$hour/24)
df$hour_sin8 = sin(2*pi*8*df$hour/24)
df$hour_sin9 = sin(2*pi*9*df$hour/24)
df$hour_sin10 = sin(2*pi*10*df$hour/24)
df$hour_sin11 = sin(2*pi*11*df$hour/24)
df$hour_sin12 = sin(2*pi*12*df$hour/24)
df$hour_cos1 = cos(2*pi*1*df$hour/24)
df$hour_cos2 = cos(2*pi*2*df$hour/24)
df$hour_cos3 = cos(2*pi*3*df$hour/24)
df$hour_cos4 = cos(2*pi*4*df$hour/24)
df$hour_cos5 = cos(2*pi*5*df$hour/24)
df$hour_cos6 = cos(2*pi*6*df$hour/24)
df$hour_cos8 = cos(2*pi*8*df$hour/24)
df$hour_cos9 = cos(2*pi*9*df$hour/24)
df$hour_cos10 = cos(2*pi*10*df$hour/24)
df$hour_cos11 = cos(2*pi*11*df$hour/24)
df$hour_cos12 = cos(2*pi*12*df$hour/24)


df$day_week = wday(df$Date)
df$day_week_sin1 = sin(2*pi*1*df$day_week/7)
df$day_week_sin2 = sin(2*pi*2*df$day_week/7)
df$day_week_sin3 = sin(2*pi*3*df$day_week/7)
df$day_week_sin4 = sin(2*pi*4*df$day_week/7)
df$day_week_cos1 = cos(2*pi*1*df$day_week/7)
df$day_week_cos2 = cos(2*pi*2*df$day_week/7)
df$day_week_cos3 = cos(2*pi*3*df$day_week/7)
df$day_week_cos4 = cos(2*pi*4*df$day_week/7)


df$day_week_2 = (df$day_week)^2
df$day_week_3 = (df$day_week)^3

df$wday = ifelse(df$day_week %in% c(2,3,4,5),1,0)
df$month = month(df$Date)
df$month_sin1 = sin(2*pi*1*df$month/12)
df$month_sin2 = sin(2*pi*2*df$month/12)
df$month_sin3 = sin(2*pi*3*df$month/12)
df$month_sin4 = sin(2*pi*4*df$month/12)
df$month_sin5 = sin(2*pi*5*df$month/12)
df$month_sin6 = sin(2*pi*6*df$month/12)
df$month_cos1 = cos(2*pi*1*df$month/12)
df$month_cos2 = cos(2*pi*2*df$month/12)
df$month_cos3 = cos(2*pi*3*df$month/12)
df$month_cos4 = cos(2*pi*4*df$month/12)
df$month_cos5 = cos(2*pi*5*df$month/12)
df$month_cos6 = cos(2*pi*6*df$month/12)

df$day_month = mday(df$Date)
df$day_year = yday(df$Date)
df$day_year_2 = (df$day_year)^2
df$day_year_3 = (df$day_year)^3
df$day_year_sin1 = sin(2*pi*1*df$day_year/365.25)
df$day_year_sin2 = sin(2*pi*2*df$day_year/365.25)
df$day_year_sin3 = sin(2*pi*90*df$day_year/365.25)
df$day_year_sin4 = sin(2*pi*180*df$day_year/365.25)
df$day_year_sin5 = sin(2*pi*270*df$day_year/365.25)
df$day_year_cos1 = cos(2*pi*1*df$day_year/365.25)
df$day_year_cos2 = cos(2*pi*2*df$day_year/365.25)
df$day_year_cos3 = cos(2*pi*90*df$day_year/365.25)
df$day_year_cos4 = cos(2*pi*180*df$day_year/365.25)
df$day_year_cos5 = cos(2*pi*270*df$day_year/365.25)


df$quarter = quarter(df$Date)
df$year = year(df$Date)
df$warm = ifelse(df$month %in% c(5,6,7,8,9), 1, 0)

df$trend = seq(1,n,1)
df$cca = ifelse(as.Date(df$Date) > as.Date("2019-03-01"), 1,0)

df$LAX_High_2 = (df$LAX_High)^2
df$LAX_High_3 = (df$LAX_High)^3
df$lax_low_2 = (df$lax_low)^2
df$lax_low_3 = (df$lax_low)^3
df$RIV_High_2 = (df$RIV_High)^2
df$RIV_High_3 = (df$RIV_High)^3
df$riv_low_2 = (df$riv_low)^2
df$riv_low_3 = (df$riv_low)^3

rm( all, average, average2, temp, temp_d)



##############################################################################

########################## 4. Build the model

## use the past 90 days' data to build the model, 3 month's data prediction is better than one year data, one year is two much
start_2017 = (365+365+366)*24+1
start_2018 = (365+365+366+365)*24 +1
start_2019 = (365+365+366+365+365)*24 +1

ntrain = 24*3*365 +8
nvalid = 40
len = ntrain+nvalid
# "riv_low_2" ,"lax_low_2","WJF_High","trm_Low","TRM_High","lax_low","riv_low",
# remove  = c("temperature","avg","mondayhour","date_r","trm_low","CQT_High","cqt_low","wjf_Low","Date" , "load","zone" )
# variables = colnames(df)[! colnames(df) %in% remove]
# variables = c('l2','l7','l2_2','l14','RIV_High','l21','y1_1b','RIV_High_2','w4','riv_low','hour_sin2','w24','WJF_High','day_week_cos1','hour','RIV_High_3','LAX_High','w20','holiday','hour_sin1','l6','hour_cos3','day_week_sin2','day_year_sin1','riv_low_2','w16','lax_low','TRM_High','day_week','day_year','day_week_sin1','l4','trend','day_year_sin2','day_year_cos1','w12','hour_sin','hour_cos1','day_year_cos2','LAX_High_3','month_cos2','lax_low_2','LAX_High_2','month_sin1','l2_3','day_month','y1_1c','y2_1b','hour_cos','day_year_sin5','y1_1a','hour_cos10','w8','l5','hour_sin5','y2','month_cos4','month_sin5','hour_2','wday','month_sin4','y1','day_year_sin4','hour_cos2','day_week_sin3','trm_Low','day_week_sin4','lax_low_3','day_year_cos3','month_cos6','day_week_2','day_year_sin3','day_year_cos5','month_cos5','day_year_2','l3','warm','hour_sin11','day_year_3','day_week_cos3','hour_3')
variables = c("month_cos6","month_cos5","month_cos4","month_cos3","month_cos2","month_cos1","month_sin6","month_sin5","month_sin4","month_sin3","month_sin2","month_sin1","lax_low_3","riv_low_3","RIV_High_3","LAX_High_3","y1","y1_1c","y1_1a","y1_1b","y2","y2_1b","l2_2","l2","l7","l14","RIV_High","RIV_High_2","w4","hour_sin2","l21","w24","WJF_High","riv_low","hour","day_week_cos1","w20","LAX_High","hour_cos3","hour_sin","riv_low_2","TRM_High","day_year_sin1","lax_low","holiday","day_year","l6","day_week_sin1","day_week_sin2","day_year_sin2","hour_2","day_week","w12","trend","l3","day_year_cos2","wday","w8","LAX_High_2","w16","lax_low_2","hour_cos","day_month","l4","hour_sin1","day_year_cos1","day_year_sin5","hour_cos10","hour_sin5","hour_sin3","l2_3","l5","day_year_sin4","trm_Low","day_week_sin3","day_year_cos5","hour_cos2","day_year_sin3","day_year_2","day_year_cos3","day_year_cos4","day_week_sin4","day_week_2","hour_sin11","month","hour_cos1","hour_sin12","hour_sin6","hour_sin4","hour_cos4")  

############################################################################################################


#################################### for year 2017:
result = data.frame(Date=as.Date(character()), MAPE=double(),RMSE= double(), peak= integer(), peak_pred = integer(), stringsAsFactors=FALSE) 
load_record = data.frame()

i = 0  ## 0 is Jan-1, 24 is Jan-2, etc

for( i in seq(24*0,24*364,24)){

  initial.df = df[(start_2017+24-len+i):(start_2017-len+ntrain-1+i+24),]
  train.df = initial.df[1:(24*(3*365-1)-8),]
  valid.df = initial.df[(24*(3*365-1)-8+1):ntrain,]
  test.df = df[(start_2017+24-len+ntrain+i):(start_2017+24-1+i),]
  
  predictors_initial = as.matrix(initial.df[,variables])
  response_initial = initial.df$load
  initial_m = xgb.DMatrix(predictors_initial,label=response_initial)
  
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
                gamma=0.6,colsample_bytree = 0.9,colsample_bylevel = 0.9, lambda = 0.1,
                min_child_weight = 2,objective = 'reg:linear',eval_metric = 'rmse')
  
  watchlist = list(train = train_m, eval = valid_m)
  
  micro_model = xgb.train(data = train_m, params =  params, watchlist = watchlist,nrounds=120, early_stopping_rounds = 10,verbose = 0)
  predicts = predict(micro_model,test_m, type = 'response')
  
  resid = response_initial - predict(micro_model,initial_m, type = 'response')
  resid.ts = ts(resid,frequency = 24)
  # tsdisplay(resid_pred$residuals)
  resid_model = Arima(resid.ts, order = c(1,0,1), seasonal = c(1,0,0))
  resid_pred = forecast(resid_model, h = nvalid)  
  final_pred = predicts + resid_pred$mean  
  accurate = data.frame( accuracy(response_test[17:40], final_pred[17:40]))
  
  p = final_pred[17:40]
  t = response_test[17:40]
  p_pred = paste(which(p == max(p)) -1, collapse=" ")
  p_t = paste(which(t == max(t)) -1,collapse=" ")
  
result[(i/24+1),"MAPE"] = accurate$MAPE
result[(i/24+1),"RMSE"] = accurate$RMSE
result[(i/24+1), "Date"] = as.Date("2017-01-01")+i/24
result[(i/24+1), "peak"] = p_t
result[(i/24+1), "peak_pred"] = p_pred
print(i/24)

new = data.frame(Date = test.df$Date[17:40], load = t, predict = p)
load_record = rbind(load_record, new)

}

ggplot(data = result, aes(x = Date, y = MAPE))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("MAPE in 2017")+
  theme_bw()

mean(result$MAPE, na.rm = T)

write.csv(result, 'MAPE_2017.csv')
write.csv(load_record, "load_record_2017.csv")



#################################### for year 2018:
result2 = data.frame(Date=as.Date(character()), MAPE=double(),RMSE= double(), peak= integer(), peak_pred = integer(), stringsAsFactors=FALSE) 
load_record2 = data.frame()

i = 0  ## 0 is Jan-1, 24 is Jan-2, etc

for( i in seq(24*0,24*364,24)){
  
  initial.df = df[(start_2018+24-len+i):(start_2018-len+ntrain-1+i+24),]
  train.df = initial.df[1:(24*(3*365-1)-8),]
  valid.df = initial.df[(24*(3*365-1)-8+1):ntrain,]
  test.df = df[(start_2018+24-len+ntrain+i):(start_2018+24-1+i),]
  
  predictors_initial = as.matrix(initial.df[,variables])
  response_initial = initial.df$load
  initial_m = xgb.DMatrix(predictors_initial,label=response_initial)
  
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
                gamma=0.6,colsample_bytree = 0.9,colsample_bylevel = 0.9, lambda = 0.1,
                min_child_weight = 2,objective = 'reg:linear',eval_metric = 'rmse')
  
  watchlist = list(train = train_m, eval = valid_m)
  
  micro_model = xgb.train(data = train_m, params =  params, watchlist = watchlist,nrounds=120, early_stopping_rounds = 10,verbose = 0)
  predicts = predict(micro_model,test_m, type = 'response')
  
  resid = response_initial - predict(micro_model,initial_m, type = 'response')
  resid.ts = ts(resid,frequency = 24)
  # tsdisplay(resid_pred$residuals)
  resid_model = Arima(resid.ts, order = c(1,0,1), seasonal = c(1,0,0))
  resid_pred = forecast(resid_model, h = nvalid)  
  final_pred = predicts + resid_pred$mean  
  accurate = data.frame( accuracy(response_test[17:40], final_pred[17:40]))
  
  p = final_pred[17:40]
  t = response_test[17:40]
  p_pred = paste(which(p == max(p)) -1, collapse=" ")
  p_t = paste(which(t == max(t)) -1,collapse=" ")
  
  result2[(i/24+1),"MAPE"] = accurate$MAPE
  result2[(i/24+1),"RMSE"] = accurate$RMSE
  result2[(i/24+1), "Date"] = as.Date("2018-01-01")+i/24
  result2[(i/24+1), "peak"] = p_t
  result2[(i/24+1), "peak_pred"] = p_pred
  print(i/24)
  
  new = data.frame(Date = test.df$Date[17:40], load = t, predict = p)
  load_record2 = rbind(load_record2, new)
  
}

ggplot(data = result2, aes(x = Date, y = MAPE))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("MAPE in 2018")+
  theme_bw()

mean(result2$MAPE, na.rm = T)

write.csv(result2, 'MAPE_2018.csv')
write.csv(load_record2, "load_record_2018.csv")



#################################### for year 2019:
result3 = data.frame(Date=as.Date(character()), MAPE=double(),RMSE= double(), peak= integer(), peak_pred = integer(), stringsAsFactors=FALSE) 
load_record3 = data.frame()

i = 0  ## 0 is Jan-1, 24 is Jan-2, etc

for( i in seq(24*0,24*364,24)){
  
  initial.df = df[(start_2019+24-len+i):(start_2019-len+ntrain-1+i+24),]
  train.df = initial.df[1:(24*(3*365-1)-8),]
  valid.df = initial.df[(24*(3*365-1)-8+1):ntrain,]
  test.df = df[(start_2019+24-len+ntrain+i):(start_2019+24-1+i),]
  
  predictors_initial = as.matrix(initial.df[,variables])
  response_initial = initial.df$load
  initial_m = xgb.DMatrix(predictors_initial,label=response_initial)
  
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
                gamma=0.6,colsample_bytree = 0.9,colsample_bylevel = 0.9, lambda = 0.1,
                min_child_weight = 2,objective = 'reg:linear',eval_metric = 'rmse')
  
  watchlist = list(train = train_m, eval = valid_m)
  
  micro_model = xgb.train(data = train_m, params =  params, watchlist = watchlist,nrounds=120, early_stopping_rounds = 10,verbose = 0)
  predicts = predict(micro_model,test_m, type = 'response')
  
  resid = response_initial - predict(micro_model,initial_m, type = 'response')
  resid.ts = ts(resid,frequency = 24)
  # tsdisplay(resid_pred$residuals)
  resid_model = Arima(resid.ts, order = c(1,0,1), seasonal = c(1,0,0))
  resid_pred = forecast(resid_model, h = nvalid)  
  final_pred = predicts + resid_pred$mean  
  accurate = data.frame( accuracy(response_test[17:40], final_pred[17:40]))
  
  p = final_pred[17:40]
  t = response_test[17:40]
  p_pred = paste(which(p == max(p)) -1, collapse=" ")
  p_t = paste(which(t == max(t)) -1,collapse=" ")
  
  result3[(i/24+1),"MAPE"] = accurate$MAPE
  result3[(i/24+1),"RMSE"] = accurate$RMSE
  result3[(i/24+1), "Date"] = as.Date("2019-01-01")+i/24
  result3[(i/24+1), "peak"] = p_t
  result3[(i/24+1), "peak_pred"] = p_pred
  print(i/24)
  
  new = data.frame(Date = test.df$Date[17:40], load = t, predict = p)
  load_record3 = rbind(load_record3, new)
  
}

ggplot(data = result3, aes(x = Date, y = MAPE))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("MAPE in 2019")+
  theme_bw()

mean(result3$MAPE, na.rm = T)

write.csv(result3, 'MAPE_2019.csv')
write.csv(load_record3, "load_record_2019.csv")
