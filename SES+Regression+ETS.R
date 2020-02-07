####################################### Three models in this file ###################################################

library(forecast)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
library(chron)
library(timeDate)
library(prophet)

setwd("~/2019 fall/Time Series/final project")
###############################  1. clean data for missing value of load ###################################################
df = read_excel("20140101-20190901 SCE & CAISO Actual Load  9 27 2019.xlsx", 
                sheet = "SCE Load Only", col_names = T)  # 49342 records
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

df%>%
  ggplot(aes(x=Date,y=load))+
  geom_line()

#################################################################################################################################

########################################## 2 read hourly temperature data    ###################################################

temp = read.csv("temp.csv")
temp$date = as.POSIXct(temp$date,format = "%Y-%m-%d %H:%M",tz = "UTC")
#temp$date = as.character(temp$date)
temp = temp %>%
  group_by(date) %>%
  summarise(temperature = mean(Means))


#substring(temp$date,1,2) <- "20"

df = left_join(df, temp, by = c("Date"= "date"))
sum(is.na(df))
#df[which(is.na(df$temperature)),"Date"]
#plot(df$temperature[1:(24*60)], type = 'l')

########################################## 2.2 read daily temperature data (not used in below's model) ###################################################
temp_d = read_excel("SCETemps14-19.xlsx", col_names = T)

temp_d$Date = as.Date(temp_d$Date)
colnames(temp_d) = gsub(" ", "", colnames(temp_d))
colnames(temp_d) = gsub("-", "_", colnames(temp_d))

df$date_r = as.Date(df$Date)
df = left_join(df, temp_d, by = c("date_r" = "Date"))

#################################################################################################################################



############################################### 3. Build variables##############################################################

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

######## 3.3 create hour of day, day of week, day of month, etc..

hlist <- c("USChristmasDay","USGoodFriday","USIndependenceDay","USLaborDay",
           "USNewYearsDay","USThanksgivingDay")        
myholidays  <- dates(as.character(holiday(2014:2019,hlist)),format="Y-M-D")

df$holiday = as.integer(is.holiday(df$Date,myholidays))

df$hour = hour(df$Date)
df$day_week = wday(df$Date)
df$wday = ifelse(df$day_week<=5,1,0)
df$monday =  ifelse(df$day_week==1,1,0)
df$tuesday = ifelse(df$day_week==2,1,0)
df$sunday = ifelse(df$day_week==7,1,0)
df$day_month = mday(df$Date)
df$month = month(df$Date)
df$day_year = yday(df$Date)
df$quarter = quarter(df$Date)
df$trend = seq(1,n,1)
df$cca = ifelse(as.Date(df$Date) > as.Date("2019-03-01"), 1,0)

df$four = as.integer(ifelse(df$hour %% 24 == 4, 1,0))
df$seven = as.integer(ifelse(df$hour %% 24 == 7, 1,0))
df$ten = as.integer(ifelse(df$hour %% 20 == 10, 1,0))
df$eleven = as.integer(ifelse(df$hour %% 22 == 11, 1,0))
df$twelve = as.integer(ifelse(df$hour %% 24 == 12, 1,0))
df$thirteen = as.integer(ifelse(df$hour %% 24 == 13, 1,0))
df$fourteen = as.integer(ifelse(df$hour %% 24 == 14, 1,0))
df$fifteen = as.integer(ifelse(df$hour %% 24 == 15, 1,0))
df$eighteen = as.integer(ifelse(df$hour %% 18 == 0, 1,0))
df$nineteen = as.integer(ifelse(df$hour %% 19 == 0, 1,0))
df$twenty = as.integer(ifelse(df$hour %% 20 == 0, 1,0))


############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################
############################################################################################################################################################


###################################################### 4. Build the model ##################################################################################

## use the past 90 days' data to build the model, 3 month's data prediction is better than one year data, one year is two much
start_2017 = (365+365+366)*24+1
start_2018 = (365+365+366+365)*24 +1
start_2019 = (365+365+366+365+365)*24 +1

ntrain = 24*365+8
nvalid = 40
len = ntrain+nvalid

################################# simple exponential smoothing + regression as second layer #######################################
## for year 2017:
result = data.frame(Date=as.Date(character()), MAPE=double(), stringsAsFactors=FALSE) 

i = 0  ## 0 is Jan-1, 24 is Jan-2, etc
# For loop
for( i in seq(24*0,24*365*3,24)){

y= ts(df$load[(start_2017+24-len+i):(start_2017+i+24-1)])
y
train.ts = window(y, end = c(1, ntrain))
valid.ts = window(y, start = c(1,ntrain+1))

train.df = df[(start_2017+24-len+i):(start_2017-len+ntrain-1+i+24),]
valid.df = df[(start_2017+24-len+ntrain+i):(start_2017+24-1+i),]

simple_exponential<-ses(train.ts, h=40, alpha=0.1, initial="simple")

pred_ses<-predict(simple_exponential,n.ahead=nvalid)

##################################### Second layer, Add arima can increase the accuracy ############################################
resid = pred_ses$residuals
train.df$resid.ts=resid

formula = as.formula(resid.ts~ temperature+l2+l3+l7+wday+sunday+holiday+monday:eleven+monday:twelve+monday:thirteen+monday:eighteen+sunday:eleven+sunday:twelve+sunday:thirteen+sunday:eighteen+poly(hour,3) +I(sin(2*pi*((hour+12)%%24)/24))+I(cos(2*pi*((hour+12)%%24)/24))+I(sin(2*pi*day_week/7))+I(cos(2*pi*day_week/7))+day_week+ I(sin(2*pi*day_month/30))+I(cos(2*pi*day_month/30))  +I(sin(2*pi*day_year/365.25))+I(cos(2*pi*day_year/365.25))+cca)
lm_model=lm(formula,data=train.df)

pred_lm = predict(lm_model,newdata = valid.df, h=nvalid, level=0)

final_pred = pred_lm+ts(pred_ses$mean,start = c(1,ntrain+1))
accurate = data.frame(accuracy(final_pred[17:40], valid.ts[17:40])) 
accurate$RMSE

result[(i/24+1),"RMSE"] = accurate[2]
result[(i/24+1),'MAPE'] = accurate[5]
result[(i/24+1), "Date"] = as.Date("2017-01-01")+i/24

}

ggplot(data = result, aes(x = Date, y = RMSE))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("RMSE in 2017")+
  theme_bw()

mean(result$RMSE, na.rm = T)  

############################# 4.2  ets  ######################################################################################

## for year 2017:
result = data.frame(Date=as.Date(character()), MAPE=double(), stringsAsFactors=FALSE) 

i = 0  ## 0 is Jan-1, 24 is Jan-2, etc
# For loop
for( i in seq(24*0,24*365,24)){
  
  y= ts(df$load[(start_2017+24-len+i):(start_2017+i+24-1)])
  train.ts = window(y, end = c(1, ntrain))
  valid.ts = window(y, start = c(1,ntrain+1))

  train.df = df[(start_2017+24-len+i):(start_2017-len+ntrain-1+i+24),]
  valid.df = df[(start_2017+24-len+ntrain+i):(start_2017+24-1+i),]
  
  exponential_smoothing<-ets(train.ts, damped = T,lambda='auto',model='ZZZ')
  pred_ets<-predict(exponential_smoothing,h=nvalid,level=95)
  accurate=accuracy(pred_ets$mean[17:40],valid.ts[17:40])
  
  final_pred = ts(pred_ets$mean,start = c(1,ntrain+1))
  
  result[(i/24+1),"MAPE"] = accurate[5]
  result[(i/24+1), "Date"] = as.Date("2019-01-01")+i/24
  
}

ggplot(data = result, aes(x = Date, y = MAPE))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("MAPE in 2019 Jan，Feb & March")+
  theme_bw()

mean(result$MAPE, na.rm = T)  


########################### 4.3  ses  #######################################################################################
result = data.frame(Date=as.Date(character()), MAPE=double(), stringsAsFactors=FALSE) 

i = 0  ## 0 is Jan-1, 24 is Jan-2, etc
# For loop
for( i in seq(24*0,24*365*3,24)){
  
  y= ts(df$load[(start_2017+24-len+i):(start_2017+i+24-1)])
  train.ts = window(y, end = c(1, ntrain))
  valid.ts = window(y, start = c(1,ntrain+1))
  
  train.df = df[(start_2017+24-len+i):(start_2017-len+ntrain-1+i+24),]
  valid.df = df[(start_2017+24-len+ntrain+i):(start_2017+24-1+i),]
  
  simple_exponential<-ses(train.ts, h=40, alpha=0.1, initial="simple")
  
  pred_ses<-predict(simple_exponential,n.ahead=nvalid)

  accurate = data.frame(accuracy(pred_ses[17:40], valid.ts[17:40])) 
  
  result[(i/24+1),"RMSE"] = accurate[2]
  result[(i/24+1),'MAPE'] = accurate[5]
  result[(i/24+1), "Date"] = as.Date("2017-01-01")+i/24
  
}

ggplot(data = result2, aes(x = Date, y = MAPE))+
  geom_line()+
  geom_point()+
  geom_hline(yintercept = 2, col = 'blue')+
  ggtitle("MAPE in 2019 Jan，Feb & March")+
  theme_bw()

mean(result2$MAPE, na.rm = T)  

############################  4.4  components plot #################################################################################
df2=df
colnames(df2)[1]<-'ds'
colnames(df2)[2]<-'y'
df2=df2[,c(1,2)]

m_prophet <- prophet(df2)
future <- make_future_dataframe(m_prophet, periods = 40)
forecast <- predict(m_prophet, future)
plot(m_prophet,forecast)
prophet_plot_components(m_prophet, forecast)

### prophet runs so slow so we didn't build complicated for loop based on prophet


