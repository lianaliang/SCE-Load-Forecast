library(forecast)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)
df = read_excel("20140101-20190901 SCE & CAISO Actual Load  9 27 2019.xlsx", sheet = "SCE Load Only", col_names
 = T)

df$Date = as.POSIXct(as.character(df$Date), format = "%Y-%m-%d %H:%M")
temp = read.csv("LAX-Temperature-1.csv")
temp = temp %>%
  group_by(date) %>%
  summarise(temperature = mean(tmpf))
temp$date = as.POSIXct(as.character(temp$date), tryFormats = c("%m/%d/%Y %H:%M"))
temp$date = as.character(temp$date)
substring(temp$date,1,2) <- "20"
temp$date = as.POSIXct(temp$date,format = "%Y-%m-%d %H:%M")
df = left_join(df, temp, by = c("Date"= "date"))

## create hour of day, day of week, day of month, etc..
n = nrow(df)
df$hour = hour(df$Date)
df$day_week = wday(df$Date)
df$day_month = mday(df$Date)
df$month = month(df$Date)
df$day_year = yday(df$Date)
df$quarter = quarter(df$Date)
df$trend = seq(1,n,1)
df$cca = ifelse(as.Date(df$Date) > as.Date("2019-03-01"), 1,0)
df$six = as.integer(df$hour/6)


## try for the first 2 months, 3 month's data prediction is better than one year data, one year is two much
ntrain = 24*90
nvalid = 40
from_n = ntrain+1
to_n = ntrain+nvalid
len = ntrain+nvalid

y= ts(df$load[1:len])
train.ts = window(y, end = c(1, ntrain))
valid.ts = window(y, start = c(1,ntrain+1))
remove  = c("Date" , "load","zone" )
variables = colnames(df)[! colnames(df) %in% remove]

train.df = df[1:ntrain,]
valid.df = df[from_n:to_n,]

paste("train.ts", paste(variables, collapse = "+"), sep = "~")
formula = as.formula("train.ts~temperature+I(hour^2)+hour + I(sin(pi*hour/24))+I(sin(pi*day_week/24))+I(day_week^2) +day_week+ I(sin(pi*day_month/30))+I(day_month^2) +day_month+month+ I(sin(pi*month/12)) +day_year+I(sin(pi*day_year/365.25))+quarter+trend+cca+ six + I(six^2)")

lm_model = tslm(formula, data = train.df)

pred_lm = forecast(lm_model, newdata = valid.df,h = nvalid, level = 0)
summary(pred_lm)
accuracy(pred_lm$mean[17:40], valid.ts[17:40]) ## MAPE for linear regression is now  3.725225

# define a function to plot Acf, pacf
draw_acf = function(residual){
  par(mfrow= c(1,2))
  Acf(residual)
  Pacf(residual)
}

par(mfrow= c(1,1))
plot(pred_lm)
checkresiduals(pred_lm$residuals)


#### Second layer, Add arima can increase the accuracy
resid = pred_lm$residuals
draw_acf(resid)

resid_ts = ts(resid,frequency = 24)
ndiffs(resid_ts)
ndiffs(diff(resid_ts, lag = 1))
nsdiffs(resid_ts)
nsdiffs(diff(resid_ts, lag = 24))

res_arima = Arima(resid_ts, order = c(2,0,1), seasonal = c(2,1,0)) # need some time
arima_pred = forecast(res_arima, h = nvalid)

summary(res_arima)
draw_acf(res_arima$residuals)  ## don't know why there still have some significant lags


final_pred = pred_lm$mean + ts(arima_pred$mean,start = c(1,ntrain+1))
accuracy(final_pred[17:40], valid.ts[17:40])  # order = c(2,0,1), seasonal = c(2,1,0)) MAPE now is 1.721143


checkresiduals(res_arima$residuals)

