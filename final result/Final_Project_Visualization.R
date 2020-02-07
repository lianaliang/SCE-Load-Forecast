library(dplyr)
library(ggplot2)
library(dygraphs)
library(xts)

predict_2017 = read.csv("load_record_2017.csv")
predict_2018 = read.csv("load_record_2018.csv")
predict_2019 = read.csv("load_record_2019.csv")

# rownames(predict_2017) = predict_2017$Date
# predict_2017 = predict_2017 %>% select(-X, -Date)

datetimes_2017 <- seq.POSIXt(as.POSIXct("2017-01-01 00:00", tz="UTC"),
                             +                         as.POSIXct("2017-12-31 23:00", tz="UTC"), by="hour")
load_2017 <- xts(predict_2017$load,order.by = datetimes_2017, tz="UTC")
predseries_2017 <- xts(predict_2017$predict,order.by = datetimes_2017, tz="UTC")
predict_2017 = cbind(load_2017, predseries_2017)

dygraph(predict_2017, main = "Load Prediction for Year 2017") %>%
  dySeries("load_2017",stepPlot = F,label = "Load", color = 'blue') %>%
  dySeries("predseries_2017", stepPlot = F,label = "Prediction", color = 'red') %>%
  dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE,drawPoints = TRUE, pointSize = 1.5) %>%
  dyAxis("y", label = "Load") %>%
  dyRangeSelector(height = 20)


## 2018

datetimes_2018 <- seq.POSIXt(as.POSIXct("2018-01-01 00:00", tz="UTC"),
                             +                         as.POSIXct("2018-12-31 23:00", tz="UTC"), by="hour")
load_2018 <- xts(predict_2018$load,order.by = datetimes_2018, tz="UTC")
predseries_2018 <- xts(predict_2018$predict,order.by = datetimes_2018, tz="UTC")
predict_2018 = cbind(load_2018, predseries_2018)

dygraph(predict_2018, main = "Load Prediction for Year 2018") %>%
  dySeries("load_2018",stepPlot = F,label = "Load", color = 'blue') %>%
  dySeries("predseries_2018", stepPlot = F,label = "Prediction", color = 'red') %>%
  dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE,drawPoints = TRUE, pointSize = 1.5) %>%
  dyAxis("y", label = "Load") %>%
  dyRangeSelector(height = 20)


## 2019

datetimes_2019 <- seq.POSIXt(as.POSIXct("2019-01-01 00:00", tz="UTC"),
                             +                         as.POSIXct("2019-09-01 23:00", tz="UTC"), by="hour")
load_2019 <- xts(predict_2019$load[1:5856],order.by = datetimes_2019, tz="UTC")
predseries_2019 <- xts(predict_2019$predict[1:5856],order.by = datetimes_2019, tz="UTC")
predict_2019 = cbind(load_2019, predseries_2019)

dygraph(predict_2019, main = "Load Prediction for Year 2019") %>%
  dySeries("load_2019",stepPlot = F,label = "Load", color = 'blue') %>%
  dySeries("predseries_2019", stepPlot = F,label = "Prediction", color = 'red') %>%
  dyOptions(axisLineWidth = 1.5, fillGraph = FALSE, drawGrid = FALSE,drawPoints = TRUE, pointSize = 1.5) %>%
  dyAxis("y", label = "Load") %>%
  dyRangeSelector(height = 20)

