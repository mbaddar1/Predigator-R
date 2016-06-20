calc.own.ped.ts<-function(y.ts,own.price,method = "auto.arima") {
  methods = c("auto.arima")
  if(!(method %in% methods)) {
    stop(cat("method:",method,"is not supported!"))
  }
  log.y.ts = log(y.ts)
  print(log.y.ts)
  log.own.price = log(own.price)
  print(log.own.price)
  model = auto.arima(x = log.y.ts,xreg = log.own.price)
  
  ped = model$coef["log.own.price"]
  ret = list()
  ret$PED = unname(ped)
  return(ret)
}