
auto.arima.cust<-function(train.df,seasonality = "my",dataTimeCol,targetCol,regressorsCols=NULL) {
  #browser()
  library(forecast)
  library(lubridate)
  startDateTime = train.df[1,dataTimeCol]
  endDateTime = train.df[nrow(train.df),dataTimeCol]
  print(class(startDateTime))
  ncol.train.df = ncol(train.df)
  if(ncol.train.df <2)
    stop("Number of train data frame columns is < 2")
  step = NA
  freq = 1
  start.vec.1 = NA
  start.vec.2 = NA
  if(seasonality == "my"){
      step = "month"
      freq = 12
      start.vec.1 = year(startDateTime)
      start.vec.2 = month(startDateTime)
    }
  if(seasonality == "qy")
  {
    step = "quarter"
    freq = 4
    start.vec.1 = year(startDateTime)
    start.vec.2 = quarter(startDateTime)
  }
  if(seasonality == "wy")
  {
    step = "week"
    freq = 1
    start.vec.1 = year(startDateTime)
    start.vec.2 = week(startDateTime)
  }
  train.ts = ts(data = train.df[,targetCol],start = c(start.vec.1,start.vec.2)
                ,frequency = freq)

  model = NA
  if(is.null(regressorsCols)) {
    model = auto.arima(x = train.ts)
  }
  else{
    model = auto.arima(x = train.ts,xreg =  train.df[,regressorsCols])
  }
  return(model)
}