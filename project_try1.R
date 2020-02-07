
library(zoo)
library(forecast)
library(ggplot2)
setwd("~/2019 fall/Time Series/final project")


################ DataImport #################
df=read.csv('SCE.csv')
inds <- seq(as.Date("2014-01-01"), as.Date("2017-12-31"), by = "day")
data=ts(df$load,frequency=365.25,start=c(2014,as.numeric(format(inds[1], "%j"))))
data
################ Smoothing ##################
# Moving Average Plot
#ma.trailing = rollmean(data, k = 30, align = "right")
#ma.center = ma(data, order = 30) ## ma is in forecast library

autoplot(data)

plot(data,bty='l',xaxt='n')
lines(ma.center,lwd=2,lty=3)
#lines(ma.trailing,lwd=2,lty=2)
#legend(2012.8,16000,c("Private Housing Starts","Centered Moving Average", "Trailing Moving Average"),
       #lty=c(1,1,2),lwd=c(1,3,2))

# Train, Test Split
# 26024	12/31/16 23:00	10283	SCE
i=0
mape=NULL
while (i<365) {
train.data=window(data,end=c(2014,26024+i))
test.data=window(data,start=c(2014,26024+i+1))

train.data=window(data,end=c(2014,26024+i))
test.data=window(data,start=c(2014,26024+i+1))

# Error, Trend, Seasonal -- ETS
M1=ets(train.data,damped = T,lambda="auto")
ets.predict<-forecast(M1,h=40)
acc_M1=data.frame(accuracy(ets.predict$mean,test.data))
mape=c(mape,acc_M1$MAPE)
i=i+1
}

mape=data.frame('forecast'=seq(365),'mape'=mape)
ggplot(data=mape,aes(x=seq(365),y=mape))+
  geom_line()

# Error, Trend, Seasonal -- ETS
M1=ets(data,damped = T,lambda="auto")
ets.predict<-forecast(M1,h=62)
accuracy(ets.predict, test.data)
plot(ets.predict)
autoplot(ets.predict)
################### NN ####################
?nnetar
M2=nnetar(data)
nnetar.predict<-forecast(M2,h=62)
plot(nnetar.predict)