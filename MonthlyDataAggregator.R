  rm(list = ls())

transactions = read.csv("./data/input/transactions.preprocessed.df.csv",stringsAsFactors = F)
#attach(transactions)
library(lubridate)
library(Hmisc)
library(forecast)
transactions$WEEK_END_DATE_2=as.Date(x = transactions$WEEK_END_DATE,format = "%d-%b-%y")
transactions$YEAR= year(transactions$WEEK_END_DATE_2)
transactions$MONTH = month(transactions$WEEK_END_DATE_2)
transactions$DAY_OF_MONTH = day(transactions$WEEK_END_DATE_2)
transactions$WEEK_OF_YEAR = week(transactions$WEEK_END_DATE_2)
#order by product UPC , STORE_NUM , WEEK_END_DATE
#browser()

transactions2 = transactions[order(transactions$UPC
                                   ,transactions$STORE_NUM
                                   ,transactions$WEEK_END_DATE_2),]
#Aggregating data on monthly basis
#UNITS => total
#Prices => average
#Promotions , total
UPCs = unique(transactions$UPC)
for(upc in UPCs)
{
  #browser()
  sub.trans = subset(transactions,UPC==upc)
  store.nums = unique(sub.trans$STORE_NUM)
  for(store.num in store.nums)
  {
    tryCatch(expr = {
      sub.trans.2 = subset(sub.trans,STORE_NUM==store.num)
      #aggregate over months
      agg = aggregate(formula = UNITS ~ MONTH+YEAR
                      ,data = sub.trans.2,FUN = sum)
      monthlyTrans = data.frame(UNITS = agg$UNITS,stringsAsFactors = F)
      
      agg = aggregate(formula = PRICE ~ MONTH+YEAR
                      ,data = sub.trans.2,FUN = mean)
      monthlyTrans = cbind(monthlyTrans,PRICE = agg$PRICE)
      agg = aggregate(formula = BASE_PRICE ~ MONTH+YEAR
                      ,data = sub.trans.2,FUN = mean)
      monthlyTrans = cbind(monthlyTrans,BASE_PRICE = agg$BASE_PRICE)
      agg = aggregate(formula = cbind(FEATURE,DISPLAY,TPR_ONLY)~MONTH+YEAR
                      ,data = sub.trans.2,FUN = sum)
      monthlyTrans = cbind(monthlyTrans,agg[,c("FEATURE","DISPLAY","TPR_ONLY")])
      monthlyTrans = cbind(monthlyTrans,MONTH = agg$MONTH)
      monthlyTrans = cbind(monthlyTrans,YEAR = agg$YEAR)
      
      #create dates based on 1-month-year
      nr = nrow(monthlyTrans)
      monthlyTrans = cbind(monthlyTrans,
                           data.frame(DATE=
                                        vector(mode = "character"
                                               ,length = nr)
                                      ,stringsAsFactors = F))
      for(i in 1:nr) {
        month = monthlyTrans[i,"MONTH"]
        year = monthlyTrans[i,"YEAR"]
        monthlyTrans[i,"DATE"] = paste("1",month,year,sep = "-")
      }
      #browser()
      fname = paste(upc,store.num,"transactions.csv",sep = "_")
      fname = paste("./data/aggregated/monthly/breakfast/",fname,sep = "")
      write.table(x = monthlyTrans,file = fname,row.names = F
                  ,sep = ",",col.names = T)
      print(monthlyTrans)
    },error = function(err){
      #browser()
      message(err)
    }
      )
  }
}