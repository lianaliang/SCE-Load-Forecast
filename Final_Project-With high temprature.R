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

## Load lag
n = nrow(df)
l0 = c(df$load)
# 2-6 day lag
df$d2 = c(rep(0,48), l0)[1:n] # 2 day lag
df$d3 = c(rep(0,72), l0)[1:n] # 3 day lag
df$d4 = c(rep(0,24*4), l0)[1:n]
df$d5 = c(rep(0,24*5), l0)[1:n]
df$d6 = c(rep(0,24*6), l0)[1:n] # 6 day lag
# 1-3 week lag
df$w1 = c(rep(0,24*7), l0)[1:n] # 1 week lag
df$w2 = c(rep(0,24*14), l0)[1:n] # 2 week lag
df$w3 = c(rep(0,24*21), l0)[1:n]
# 1-6 month lag 
df$m1 = c(rep(0,24*7*4), l0)[1:n] # 1 month lag
df$m2 = c(rep(0,24*7*8), l0)[1:n]
df$m3 = c(rep(0,24*7*12), l0)[1:n]
df$m4 = c(rep(0,24*7*16), l0)[1:n]
df$m5 = c(rep(0,24*7*20), l0)[1:n]
df$m6 = c(rep(0,24*7*24), l0)[1:n]

# 41-47 hour lag (?)

## Temp lag
#T0 = c(df$temperature)
# 2-6 day lag
#df$T_d2 = c(rep(0,48), T0)[1:n] # 2 day lag
#df$T_d3 = c(rep(0,72), T0)[1:n] # 3 day lag
#df$T_d4 = c(rep(0,24*4), T0)[1:n]
#df$T_d5 = c(rep(0,24*5), T0)[1:n]
#df$T_d6 = c(rep(0,24*6), T0)[1:n] # 6 day lag
# 1-3 week lag
#df$T_w1 = c(rep(0,24*7), T0)[1:n] # 1 week lag
#df$T_w2 = c(rep(0,24*14), T0)[1:n] # 2 week lag
#df$T_w3 = c(rep(0,24*21), T0)[1:n]
# 1-6 month lag 
#df$T_m1 = c(rep(0,24*7*4), T0)[1:n] # 1 month lag
#df$T_m2 = c(rep(0,24*7*8), T0)[1:n]
#df$T_m3 = c(rep(0,24*7*12), T0)[1:n]
#df$T_m4 = c(rep(0,24*7*16), T0)[1:n]
#df$T_m5 = c(rep(0,24*7*20), T0)[1:n]
#df$T_m6 = c(rep(0,24*7*24), T0)[1:n]

# "LAX_High","lax_low" Lag
#LH0 = c(df$LAX_High)
# 2-6 day lag
df$LH_d1 = c(rep(0,24), LH0)[1:n] # 1 day lag
#df$LH_d2 = c(rep(0,48), LH0)[1:n] # 2 day lag
#df$LH_d3 = c(rep(0,72), LH0)[1:n] # 3 day lag
#df$LH_d4 = c(rep(0,24*4), LH0)[1:n]
#df$LH_d5 = c(rep(0,24*5), LH0)[1:n]
#df$LH_d6 = c(rep(0,24*6), LH0)[1:n] # 6 day lag
# 1-3 week lag
#df$LH_w1 = c(rep(0,24*7), LH0)[1:n] # 1 week lag
#df$LH_w2 = c(rep(0,24*14), LH0)[1:n] # 2 week lag
#df$LH_w3 = c(rep(0,24*21), LH0)[1:n]
# 1-6 month lag 
#df$LH_m1 = c(rep(0,24*7*4), LH0)[1:n] # 1 month lag
#df$LH_m2 = c(rep(0,24*7*8), LH0)[1:n]
#df$LH_m3 = c(rep(0,24*7*12), LH0)[1:n]
#df$LH_m4 = c(rep(0,24*7*16), LH0)[1:n]
#df$LH_m5 = c(rep(0,24*7*20), LH0)[1:n]
#df$LH_m6 = c(rep(0,24*7*24), LH0)[1:n]

LL0 = c(df$lax_low)
# 2-6 day lag
#df$LL_d1 = c(rep(0,24), LL0)[1:n] # 1 day lag
#df$LL_d2 = c(rep(0,48), LL0)[1:n] # 2 day lag
#df$LL_d3 = c(rep(0,72), LL0)[1:n] # 3 day lag
#df$LL_d4 = c(rep(0,24*4), LL0)[1:n]
#df$LL_d5 = c(rep(0,24*5), LL0)[1:n]
#df$LL_d6 = c(rep(0,24*6), LL0)[1:n] # 6 day lag
## 1-3 week lag
#df$LL_w1 = c(rep(0,24*7), LL0)[1:n] # 1 week lag
#df$LL_w2 = c(rep(0,24*14), LL0)[1:n] # 2 week lag
#df$LL_w3 = c(rep(0,24*21), LL0)[1:n]
# 1-6 month lag 
#df$LL_m1 = c(rep(0,24*7*4), LL0)[1:n] # 1 month lag
#df$LL_m2 = c(rep(0,24*7*8), LL0)[1:n]
#df$LL_m3 = c(rep(0,24*7*12), LL0)[1:n]
#df$LL_m4 = c(rep(0,24*7*16), LL0)[1:n]
#df$LL_m5 = c(rep(0,24*7*20), LL0)[1:n]
#df$LL_m6 = c(rep(0,24*7*24), LL0)[1:n]

# Creat variable for day(different area) high temprature
CQT0 = c(df$CQT_High)
df$CQT_d1 = c(rep(0,24), CQT0)[1:n] # 1 day lag
#df$CQT_d2 = c(rep(0,48), CQT0)[1:n] # 2 day lag
RIV0 = c(df$RIV_High)
df$RIV_d1 = c(rep(0,24), RIV0)[1:n]# 1 day lag
#df$RIV_d1_2 = (df$RIV_d1)^2
#df$RIV_d2 = c(rep(0,48), RIV0)[1:n] # 2 day lag
TRM0 = c(df$TRM_High)
df$TRM_d1 = c(rep(0,24), TRM0)[1:n] # 1 day lag
#df$TRM_d2 = c(rep(0,48), TRM0)[1:n] # 2 day lag
WJF0 = c(df$RIV_High)
df$WJF_d1 = c(rep(0,24), WJF0)[1:n] # 1 day lag
#df$WJF_d2 = c(rep(0,48), WJF0)[1:n] # 2 day lag

######## 3.3 create hour of day, day of week, day of month, etc..

hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay",
           "USNewYearsDay","USThanksgivingDay")        
myholidays  <- dates(as.character(holiday(2014:2019,hlist)),format="Y-M-D")

df$holiday = as.integer(is.holiday(df$Date,myholidays))

#  data <- cbind(data, dummy(data$time))

df$hour = hour(df$Date)
d = data.frame(dummy(df$hour))
df = cbind(df,d)

df$day_week = wday(df$Date)
d = data.frame(dummy(df$day_week))
df = cbind(df,d)

df$wday = ifelse(df$day_week<=5,1,0)

df$month = month(df$Date)
d = data.frame(dummy(df$month))
df = cbind(df,d)

df$day_year = yday(df$Date)
d = data.frame(dummy(df$day_year))
df = cbind(df,d)

df$year = year(df$Date)
d = data.frame(dummy(df$year))
df = cbind(df,d)

df$quarter = quarter(df$Date)
#d = data.frame(dummy(df$quarter))
#df = cbind(df,d)

## Season dummy ?
df$season = ifelse(df$month %in% c(3,4,5),1,
                   ifelse(df$month %in% c(6,7,8),2,
                          ifelse(df$month %in% c(9,10,11),3,
                                 ifelse(df$month %in% c(12,1,2),4,0))))
d = data.frame(dummy(df$season))
df = cbind(df,d)

df$trend = seq(1,n,1)
df$cca = ifelse(as.Date(df$Date) > as.Date("2019-03-01"), 1,0)


##############################################################################

########################## 4. Build the model

## use the past 90 days' data to build the model, 3 month's data prediction is better than one year data, one year is two much
start_2017 = (365+365+366)*24+1
start_2018 = (365+365+366+365)*24 +1
start_2019 = (365+365+366+365+365)*24 +1

ntrain = 24*2*365 +8
nvalid = 40
len = ntrain+nvalid

remove  = c("mondayhour","date_r","CQT_High","cqt_low","RIV_High","riv_low","WJF_High","wjf_Low","avg","quarter",
            "Date","load","zone","temperature","LAX_High","lax_low","TRM_High","trm_Low","hour","day_week",
            "month","year","season","day_year")
variables = colnames(df)[! colnames(df) %in% remove]
variables

result = data.frame(Date=as.Date(character()), MAPE=double(), stringsAsFactors=FALSE) 

i = 0  ## 0 is Jan-1, 24 is Jan-2, etc

## for year 2017:

for( i in seq(24*120,24*211,24)){

  initial.df = df[(start_2017+24-len+i):(start_2017-len+ntrain-1+i+24),]
  train.df = initial.df[1:(24*723),]
  valid.df = initial.df[(24*723+1):ntrain,]
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
                gamma=0.6,colsample_bytree = 0.9,colsample_bylevel = 0.9, lambda = 0.1,
                min_child_weight = 2,objective = 'reg:linear',eval_metric = 'rmse')
  
  watchlist = list(train = train_m, eval = valid_m)
  
  micro_model = xgb.train(data = train_m, params =  params, watchlist = watchlist,nrounds=100, 
                          early_stopping_rounds = 10,verbose = 0)
  
  predicts = predict(micro_model,test_m, type = 'response')
  accurate = data.frame( accuracy(response_test[17:40], predicts[17:40]))
 

result[(i/24+1),"MAPE"] = accurate$MAPE
result[(i/24+1), "Date"] = as.Date("2017-01-01")+i/24
print(i/24)
}

ggplot(data = result, aes(x = Date, y = MAPE))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("MAPE in 2017")+
  theme_bw()

mean(result$MAPE, na.rm = T)  


