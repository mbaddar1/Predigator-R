calc.promotion.effect<-function(y.ts,promotions) {
  browser()
  library(forecast)
  mean.y = mean(as.vector(y.ts),na.rm = T)
  model = arima(x = y.ts,xreg = promotions)
  promo.names = colnames(promotions)
  ret = list()
  for(name in promo.names) {
    promo.coef = model$coef[name]
    promo.effect = promo.coef / mean.y
    ret[[name]] = promo.effect
  }
  return(ret)
}