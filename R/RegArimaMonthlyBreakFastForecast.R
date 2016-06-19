rm(list = ls())

library(forecast)
library(lubridate)
source("./R/custAutoArima.R")
dir = "./data/demo"
files = list.files(path = dir,pattern = "*.csv")
for(file in files) {
  browser()
  f = paste(dir,"/",file,sep = "")
  t = read.csv(file = f,header = T,sep = ",",stringsAsFactors = F)
  nr = nrow(t)
  t$DATE_2 = as.Date(x = t$DATE,format = "%d-%m-%Y")
  t$DATE_3 = as.POSIXlt(strptime(x = t$DATE_2,format = "%Y-%m-%d"))
  print(class(t$DATE_3))
  startDate = min(t$DATE_2)
  endDate = max(t$DATE_2)
  startMonth = month(startDate)
  startYear = year(startDate)
  #imputing missing values
  fullDates = seq.Date(from = startDate,to = endDate,by = '1 month')
  idx = match(fullDates,t$DATE_2)
  t2 = t[idx,]
  
  t2$UNITS = na.approx(t2$UNITS,na.rm = F)
  t2$PRICE = na.approx(t2$PRICE,na.rm = F)
  #t2$FEATURE[is.na(t2$FEATURE)] = 0
  #t2$DISPLAY[is.na(t2$DISPLAY)] = 0
  #t2$TPR_ONLY[is.na(t2$TPR_ONLY)] = 0
  
  t2$LOG_UNITS = log(t2$UNITS)
  t2$LOG_PRICE = log(t2$PRICE)
  
  ts1 = ts(data = t2$LOG_UNITS,start = c(startYear,startMonth)
           ,frequency = 12)
  model = auto.arima(x = ts1,xreg = 
                       as.matrix(t2[
                         ,c("LOG_PRICE")]))
  print(summary(model))
  h= 12
  
  f = forecast.Arima(object = model,xreg = as.matrix(t2[1:h,c("LOG_PRICE")]))
  #f = forecast.Arima(object = model,xreg = 
  #                   as.matrix(t2[1:h,c("LOG_PRICE","FEATURE"
  #                    ,"DISPLAY","TPR_ONLY")]))
  plot(f)
  print(as.vector(t(f$mean)))
  #m2 = auto.arima.cust(train.df = t2
  #                    ,seasonality = "my",dataTimeCol = "DATE_3"
  #                    ,targetCol = "LOG_UNITS"
  #                   ,regressorsCols = c("LOG_PRICE","FEATURE"
  #                                        ,"DISPLAY","TPR_ONLY"))
  #print(summary(m2))
  
  #f2 = forecast.Arima(object = m2,xreg = 
  #                      as.matrix(t2[1:h,c("LOG_PRICE","FEATURE"
  #                                         ,"DISPLAY","TPR_ONLY")]))
  #plot(f2)
  #browser()
}