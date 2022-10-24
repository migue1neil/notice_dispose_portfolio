setwd("C:/Users/Neil/Documents/git-repos/notice_stock_portfolio")

library(data.table)
library(tidyverse)
library(lubridate)
library(zoo)

dic = c("第一款","第二款","第三款","第四款","第五款","第六款","第七款","第八款",
        "第九款","第十款","第十一款","第十二款","第十三款","第十四款" )

stock_price = fread("./tidy_up_data/合併後股價資料表_20110101_20221018.csv" , header = T , sep = "," ,
                     colClasses = list(character = c(3,4,37:39,43:44) , numeric= c(1,2,5:36,40:42) ))

stock_price = stock_price[,c(1:4,40,8,11,43,44)]

#####我需要求出每個條款的平均報酬率，t1~t20，先不看漲跌的

stock_price = stock_price %>% group_by(證券代碼) %>%
              mutate( 六日漲跌 = ifelse((調整收盤價 -lag(調整收盤價 ,5)) / lag(調整收盤價 , 5) >= 0 , "+", "-" )) %>% ungroup

#test瘦身 #選前100檔股票
# slist = unique(stock_price$證券代碼)[1:100]
# stock_price = stock_price[stock_price$證券代碼 %in% slist ,]

##### for 第二款使用 30日漲跌 60日漲跌 90日漲跌
stock_price = stock_price %>% group_by(證券代碼) %>%
     mutate( 當天收盤減開盤 = ifelse((調整收盤價 - 調整開盤價) > 0 ,"+" , "-" ),
             當天跟前30天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,29)) / lag(調整收盤價,29)) > 0 , "+" , "-"),
             當天跟前60天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,59)) / lag(調整收盤價,59)) > 0 , "+" , "-"),
             當天跟前90天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,89)) / lag(調整收盤價,89)) > 0 , "+" , "-")
             #三十日最高 = rollmax(x = 調整收盤價 , k = 10 , align = "right" , fill = NA),
             #三十日最低 = rollmax(x = -調整收盤價 , k = 10 , align = "right" , fill = NA) %>% abs ,
             )
#第二款漲跌判斷 ，要把上面的縮減到一個欄位
stock_price = stock_price %>% group_by(證券代碼) %>%
  mutate(  第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("三十", 注意交易資訊) & 當天跟前30天報酬 == "+","+","-"),
           第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("六十", 注意交易資訊) & 當天跟前60天報酬 == "+", "+" , 第二款漲跌 ) ,
           第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("九十", 注意交易資訊) & 當天跟前90天報酬 == "+", "+" , 第二款漲跌 )
)

#stock_price %>% filter(is.na(調整收盤價) == T )
                                      

###先用向量方式算出每個時間點持有t0~t20的報酬
for (ndays in 0:20){
  if (ndays == 0){ stock_price = stock_price %>% group_by(證券代碼) %>% 
                                  mutate( 持有0日報酬 = (調整開盤價-調整收盤價)/調整開盤價) } %>% ungroup #持有0日是偷看
  if(ndays != 0){
    stock_price = stock_price %>% group_by(證券代碼) %>% 
                  mutate( holding_ndays = (lead(調整開盤價, n = 1) - lead(調整收盤價, n = ndays))/lead(調整開盤價, n = 1)) %>% ungroup
    colnames(stock_price)[NCOL(stock_price)]  = paste0("持有",ndays,"日報酬")
  }
}
stock_price[,c(NCOL(stock_price)-20 :NCOL(stock_price))] = 
  stock_price[,c(NCOL(stock_price)-20:NCOL(stock_price))] %>% round(digits = 4) #把全部取小數點後四位


#算第每款不分類持有報酬，TSE
each.law.return.func = function(stock_price , market , volatility){
  no_tidy_sheet = data.table()
  for (law in dic){
  #第二款條款
    if(law == "第二款"){
      #test cc = stock_price %>% filter( conditions == "第二款" )
      if(volatility == "NA"){
        each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) )  }
      if(volatility != "NA"){
        each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) & 第二款漲跌 == volatility )}
    }
    if(law != "第二款"){
      if(volatility == "NA"){
        each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) )  }
      if(volatility != "NA"){
        each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) & 六日漲跌 == volatility )}
    }  
    sheet = data.table()
    for (i in 16:36){ #有修改的話位置要調
      cc = data.table(mean = mean(each_law[[i]] , na.rm=T ) )
      colnames(cc) = colnames(each_law[,i])
      sheet = cbind(sheet , cc )
    }
    no_tidy_sheet = rbind( no_tidy_sheet , sheet)
  }
  no_tidy_sheet = no_tidy_sheet %>% t()
  colnames(no_tidy_sheet) = dic
  no_tidy_sheet = no_tidy_sheet %>% round(4)
  return(no_tidy_sheet)
}

#不分漲跌
TSE_no_catagory = each.law.return.func(stock_price , market = "TSE" ,volatility = "NA" )
OTC_no_catagory = each.law.return.func(stock_price , market = "OTC" ,volatility = "NA" )

#分漲跌 #其實近六日漲跌幅只適用 1,3,4,5,7,11 款，其他的要想辦法刪掉
TSE_vol_upside = each.law.return.func(stock_price , market = "TSE" ,volatility = "+")
TSE_vol_downside = each.law.return.func(stock_price , market = "TSE" ,volatility = "-") 

OTC_vol_upside = each.law.return.func(stock_price , market = "OTC" ,volatility = "+")
OTC_vol_downside = each.law.return.func(stock_price , market = "OTC" ,volatility = "-")


#存檔的部分
write.csv(TSE_no_catagory , "./tidy_up_data/不分類TSE持有報酬.csv" , row.names = T , col.names = T )
write.csv(OTC_no_catagory , "./tidy_up_data/不分類OTC持有報酬.csv" , row.names = T , col.names = T )

write.csv(TSE_vol_upside , "./tidy_up_data/漲幅TSE持有報酬.csv" , row.names = T , col.names = T )
write.csv(TSE_vol_downside , "./tidy_up_data/跌幅TSE持有報酬.csv" , row.names = T , col.names = T )

write.csv(OTC_vol_upside , "./tidy_up_data/漲幅OTC持有報酬.csv" , row.names = T , col.names = T )
write.csv(OTC_vol_downside , "./tidy_up_data/跌幅OTC持有報酬.csv" , row.names = T , col.names = T )


#我想想漲跌幅要分的更金確去意點
#分漲跌幅條款好了



#cc = stock_price %>% filter(市場別 == "TSE" & grepl("第六款",conditions) )
?write.csv
