library(forecast)
library(ggplot2)
library(dplyr)
library(readxl)
library(lubridate)

LAX=read.csv('LAXtemperature_2.csv')
LAX=LAX[,c('date','tmpf')]
colnames(LAX)[2]='LAX'
WJF=read.csv('WJFtemperature_2.csv')
WJF=WJF[,c('date','tmpf')]
colnames(WJF)[2]='WJF'
RIV=read.csv('RIVtemperature_2.csv')
RIV=RIV[,c('date','tmpf')]
colnames(RIV)[2]='RIV'
CQT=read.csv('CQTtemperature_2.csv')
CQT=CQT[,c('date','tmpf')]
colnames(CQT)[2]='CQT'
TRM=read.csv('TRMtemperature_2.csv')
TRM=TRM[,c('date','tmpf')]
colnames(TRM)[2]='TRM'



LAX$date<-as.character(LAX$date)
WJF$date<-as.character(WJF$date)
RIV$date<-as.character(RIV$date)
CQT$date<-as.character(CQT$date)
TRM$date<-as.character(TRM$date)


# time series
d1 = as.POSIXct("2014-01-01 00:00:00", tz = "UTC")
d2 = as.POSIXct("2019-09-01 23:00:00", tz = "UTC")
all = data.frame(Date = seq(d1 , d2, by = "hour"))
#all = data.frame(Date = seq(as.POSIXct("2014-01-01 00:00:00") , as.POSIXct("2019-09-01 23:00:00"), by = 3600))
all$Date=as.character(all$Date)
# shape(49679,1)

all=left_join(all,LAX,by=c('Date'='date'))
all=left_join(all,WJF,by=c('Date'='date'))
all=left_join(all,RIV,by=c('Date'='date'))
all=left_join(all,CQT,by=c('Date'='date'))
all=left_join(all,TRM,by=c('Date'='date'))

all$Date <- as.POSIXct(all$Date, format =  "%Y-%m-%d %H:%M:%S",tz = "UTC")
missing=all[rowSums(is.na(all))!=0,]

all%>%
  ggplot(aes(x=Date,y=LAX))+
  geom_line()
  #geom_point(aes(x=Date,y=WJF,color='r',size='0.2'))
  
################  
  
  
# calculate the average of each day of year
all$mondayhour = paste(month(all$Date), mday(all$Date), hour(all$Date))
average = all %>%
  group_by(mondayhour) %>%
  summarise(avglax = mean(LAX,na.rm=TRUE),
            avgwjf = mean(WJF,na.rm=TRUE),
            avgriv = mean(RIV,na.rm=TRUE),
            avgcqt = mean(CQT,na.rm=TRUE),
            avgtrm = mean(TRM,na.rm=TRUE))
  
# build a dataframe with the completed time sequence 
data = left_join(all, average, by = "mondayhour")
data$LAX[is.na(data$LAX)] <- data$avglax[is.na(data$LAX)]
data$WJF[is.na(data$WJF)] <- data$avgwjf[is.na(data$WJF)]
data$RIV[is.na(data$RIV)] <- data$avgriv[is.na(data$RIV)]
data$CQT[is.na(data$CQT)] <- data$avgcqt[is.na(data$CQT)]
data$TRM[is.na(data$TRM)] <- data$avgtrm[is.na(data$TRM)]
data[which(is.na(data$TRM)),"TRM"] = average[average$mondayhour == "2 29 3", "avgtrm"]
  

  
temp=data.frame(date=data[,1], Means=rowMeans(data[,c(2,3,4,5,6)]))



temp[is.na(temp$Means),1]
sum(is.na(temp))

temp[which(is.na(temp$Means)),"date"]

temp[which(is.na(temp$Means)),]

write.csv(temp,'temp.csv')  
 



 