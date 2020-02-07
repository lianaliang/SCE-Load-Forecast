
library(dplyr)
library(imputeTS)


library(riem)

code=riem_networks()
stations=riem_stations('CA_ASOS')
df1=riem_measures('LAX',date_start = '2014-01-01',date_end = '2019-09-02')
df2=riem_measures('CQT',date_start = '2014-01-01',date_end = '2019-09-02')
df3=riem_measures('RIV',date_start = '2014-01-01',date_end = '2019-09-02')
df4=riem_measures('TRM',date_start = '2014-01-01',date_end = '2019-09-02')
df5=riem_measures('WJF',date_start = '2014-01-01',date_end = '2019-09-02')




# summary(df1)

# Fill missing value
df1=df1%>%
  select(-c(metar,skyc1,skyc2,skyc3,skyc4,wxcodes,ice_accretion_1hr,ice_accretion_3hr,ice_accretion_6hr,
            gust,peak_wind_time,skyl1,skyl2,skyl3,skyl4,p01i,peak_wind_gust,peak_wind_drct))%>%
  mutate_at(.vars = vars(tmpf,dwpf,relh,drct,sknt,alti,mslp,vsby,feel),
            .funs = list(~ na_ma(.,k=1,weighting = 'linear')))

write.csv(df1, file = "LAXtemperature.csv",row.names = FALSE)

df2=df2%>%
  select(-c(metar,skyc1,skyc2,skyc3,skyc4,wxcodes,ice_accretion_1hr,ice_accretion_3hr,ice_accretion_6hr,
            gust,peak_wind_time,skyl1,skyl2,skyl3,skyl4,p01i,peak_wind_gust,peak_wind_drct))%>%
  mutate_at(.vars = vars(tmpf,dwpf,relh,drct,sknt,alti,mslp,vsby,feel),
            .funs = list(~ na_ma(.,k=1,weighting = 'linear')))

write.csv(df2, file = "CQTtemperature.csv",row.names = FALSE)

df3=df3%>%
  select(-c(metar,skyc1,skyc2,skyc3,skyc4,wxcodes,ice_accretion_1hr,ice_accretion_3hr,ice_accretion_6hr,
            gust,peak_wind_time,skyl1,skyl2,skyl3,skyl4,p01i,peak_wind_gust,peak_wind_drct))%>%
  mutate_at(.vars = vars(tmpf,dwpf,relh,drct,sknt,alti,mslp,vsby,feel),
            .funs = list(~ na_ma(.,k=1,weighting = 'linear')))

write.csv(df3, file = "RIVtemperature.csv",row.names = FALSE)

df4=df4%>%
  select(-c(metar,skyc1,skyc2,skyc3,skyc4,wxcodes,ice_accretion_1hr,ice_accretion_3hr,ice_accretion_6hr,
            gust,peak_wind_time,skyl1,skyl2,skyl3,skyl4,p01i,peak_wind_gust,peak_wind_drct))%>%
  mutate_at(.vars = vars(tmpf,dwpf,relh,drct,sknt,alti,mslp,vsby,feel),
            .funs = list(~ na_ma(.,k=1,weighting = 'linear')))

write.csv(df4, file = "TRMtemperature.csv",row.names = FALSE)

df5=df5%>%
  select(-c(metar,skyc1,skyc2,skyc3,skyc4,wxcodes,ice_accretion_1hr,ice_accretion_3hr,ice_accretion_6hr,
            gust,peak_wind_time,skyl1,skyl2,skyl3,skyl4,p01i,peak_wind_gust,peak_wind_drct))%>%
  mutate_at(.vars = vars(tmpf,dwpf,relh,drct,sknt,alti,mslp,vsby,feel),
            .funs = list(~ na_ma(.,k=1,weighting = 'linear')))

write.csv(df5, file = "WJFtemperature.csv",row.names = FALSE)

#  I did the cleaning in Python
