# 排除處置日當天的注意股


setwd("C:/Users/Neil/Documents/git-repos/notice_stock_portfolio")

library(data.table)
library(tidyverse)
library(lubridate)
library(zoo)

dic = c("第一款","第二款","第三款","第四款","第五款","第六款","第七款","第八款",
        "第九款","第十款","第十一款","第十二款","第十三款","第十四款" )
AR.return.order = c("第0日報酬","第1日報酬","第2日報酬","第3日報酬","第4日報酬", "第5日報酬","第6日報酬","第7日報酬","第8日報酬","第9日報酬","第10日報酬" )
CAR.return.order = c("持有0日報酬","持有1日報酬","持有2日報酬","持有3日報酬","持有4日報酬", "持有5日報酬","持有6日報酬","持有7日報酬","持有8日報酬","持有9日報酬","持有10日報酬" )
vol.order = c("NA","上漲","下跌")


stock_price_c = fread("./tidy_up_data/合併後股價資料表_20110101_20221018.csv" , header = T , sep = "," ,
                     colClasses = list(character = c(3,4,37:39,43:44) , numeric= c(1,2,5:36,40:42) ))

stock_price = stock_price[,c(1:4,40,8,11,43,44,50)] #把年的變數保留下來，做敘述統計的時候好用

####瘦身#####
test_list = stock_price %>% filter( is.na(conditions)== F)　%>%　select(證券代碼) %>% unique  #只選出有處置的證券代碼
stock_price = stock_price %>% filter( 證券代碼 %in% test_list[[1]])
rm(test_list)

#先算一下注意股樣本敘述統計 / TSE市場有多少間 / OTC





# cc = stock_price = stock_price %>% filter( 市場別 == "OTC"  & grepl("第八款", conditions) )

#####我需要求出每個條款的平均報酬率，t1~t20，先不看漲跌的
# stock_price = stock_price %>% group_by(證券代碼) %>%
#               mutate( 六日漲跌 = ifelse((調整收盤價 -lag(調整收盤價 ,5)) / lag(調整收盤價 , 5) >= 0 , "上漲", "下跌" )) %>% ungroup
# 
# 
# ##### for 第二款使用 30日漲跌 60日漲跌 90日漲跌
# stock_price = stock_price %>% group_by(證券代碼) %>%
#      mutate( 當天收盤減開盤 = ifelse((調整收盤價 - 調整開盤價) > 0 ,"上漲" , "下跌" ),
#              當天跟前30天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,29)) / lag(調整收盤價,29)) > 0 , "上漲" , "下跌"),
#              當天跟前60天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,59)) / lag(調整收盤價,59)) > 0 , "上漲" , "下跌"),
#              當天跟前90天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,89)) / lag(調整收盤價,89)) > 0 , "上漲" , "下跌")
#              #三十日最高 = rollmax(x = 調整收盤價 , k = 10 , align = "right" , fill = NA),
#              #三十日最低 = rollmax(x = -調整收盤價 , k = 10 , align = "right" , fill = NA) %>% abs ,
#              )
# 
# #第二款漲跌判斷 ，要把上面的縮減到一個欄位
# stock_price = stock_price %>% group_by(證券代碼) %>%
#   mutate(  第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("三十", 注意交易資訊) & 當天跟前30天報酬 == "上漲","上漲","下跌"),
#            第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("六十", 注意交易資訊) & 當天跟前60天報酬 == "上漲", "上漲" , 第二款漲跌 ) ,
#            第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("九十", 注意交易資訊) & 當天跟前90天報酬 == "上漲", "上漲" , 第二款漲跌 )
# )
# 
# #stock_price %>% filter(is.na(調整收盤價) == T )
#    
# #####向量方式算出每個時間點的日報酬#####
# for (ndays in 0:10){
#   if (ndays == 0){ stock_price = stock_price %>% group_by(證券代碼) %>% 
#     mutate( 第0日報酬 = (調整收盤價-調整開盤價)/調整開盤價) } %>% ungroup #持有0日是偷看
#   if(ndays != 0){
#     stock_price = stock_price %>% group_by(證券代碼) %>% 
#       mutate( holding_ndays = ( lead(調整收盤價, n = ndays) - lead(調整開盤價, n = ndays) )/lead(調整開盤價, n = ndays)) %>% ungroup
#     colnames(stock_price)[NCOL(stock_price)]  = paste0("第",ndays,"日報酬")
#   }
# }
# 
# ###先用向量方式算出每個時間點持有t0~t20的報酬
# # for (ndays in 1:20){
# #   if (ndays == 0){ stock_price = stock_price %>% group_by(證券代碼) %>% 
# #                                   mutate( 持有0日報酬 = (調整收盤價-調整開盤價)/調整開盤價) } %>% ungroup #持有0日是偷看
# #   if(ndays != 0){
# #     stock_price = stock_price %>% group_by(證券代碼) %>% 
# #                   mutate( holding_ndays = ( lead(調整收盤價, n = ndays) - lead(調整開盤價, n = 1)  )/lead(調整開盤價, n = 1)) %>% ungroup
# #     colnames(stock_price)[NCOL(stock_price)]  = paste0("持有",ndays,"日報酬")
# #   }
# # }
# # stock_price[,c(NCOL(stock_price)-20 :NCOL(stock_price))] = 
# #   stock_price[,c(NCOL(stock_price)-20:NCOL(stock_price))] %>% round(digits = 4) #把全部取小數點後四位
# 
# 
# ##### 算出提前持有1天報酬走勢
#   for (ndays in 0:10){
#     if (ndays == 0){ stock_price = stock_price %>% group_by(證券代碼) %>% 
#       mutate( 持有0日報酬 = (調整收盤價-調整開盤價)/調整開盤價) } %>% ungroup #持有0日是偷看
#     if(ndays != 0){
#       stock_price = stock_price %>% group_by(證券代碼) %>% 
#                    mutate( holding_ndays = ( lead(調整收盤價, n = ndays) - lead(調整開盤價, n = 0)  )/lead(調整開盤價, n = 0)) %>% ungroup
#       colnames(stock_price)[NCOL(stock_price)]  = paste0("持有",ndays,"日報酬")
#     }
#   }
 # stock_price[,c(NCOL(stock_price)-20 :NCOL(stock_price))] = 
 #   stock_price[,c(NCOL(stock_price)-20:NCOL(stock_price))] %>% round(digits = 4) #把全部取小數點後四位

##### 敘述統計function #####
descriptive.stat.func = function(a.vector){  
  if(length(a.vector) < 1){cc = data.table(conditions = law,
                                            index = colnames(tmp[,i]) , 
                                            market = market,
                                            vol = vol,
                                            mean = NA  , sd =NA  , max = NA ,
                                            quantile_75 =NA, median = NA ,quantile_25=NA,  min = NA ,
                                            win.rate = NA,sample = length(a.vector), p.value = NA )
  }
  
  if(length(a.vector) > 1){
    cc = data.table(conditions = law,
                    index = colnames(tmp[,i]) , 
                    market = market,
                    vol = vol,
                    mean = mean(a.vector , na.rm=T ) %>% round(4) , 
                    sd = sd(a.vector , na.rm=T) %>% round(4) ,
                    max = max(a.vector , na.rm=T ) %>% round(4) ,
                    
                    quantile_75 = quantile(a.vector , probs = 0.75 , na.rm=T ) %>% round(4),
                    median = quantile(a.vector , probs = 0.5 , na.rm=T ) %>% round(4),
                    quantile_25 = quantile(a.vector , probs = 0.25 , na.rm=T ) %>% round(4), 
                    min = min(a.vector , na.rm=T ) %>% round(4) ,
                    win.rate =  (length( (which(a.vector >= 0))) / length(a.vector)) %>% round(2),
                    sample = length(a.vector),
                    p.value = t.test(a.vector)[[3]] %>% round(4)
    )
  }
  cc = cc %>% mutate(
    p.value.mark = ifelse( p.value < 0.1 , "*" , ""  ) ,
    p.value.mark = ifelse( p.value < 0.05 , "**" , p.value.mark  ),
    p.value.mark = ifelse( p.value < 0.01 , "***" , p.value.mark  )
  )
return(cc)  
}  




#根據條款(condition算報酬)
each_notice_daily_func = function(stock_price , market){   #market = "TSE"
  dic = c("第一款","第二款","第三款","第四款","第五款","第六款","第七款","第八款",
          "第九款","第十款","第十一款","第十二款","第十三款","第十四款" )
  vol.list = c("NA","上漲","下跌")
  big_sheet = data.table()
  for(law in dic){
    vol.sheet = data.table()
    for(vol in vol.list){ #篩選出股票順便更具第二款分類
      if (law == "第二款")
        if(vol == "NA"){
          tmp = stock_price %>%　filter( grepl( law , conditions) & 市場別 == market )
        }
        if(vol != "NA"){
          tmp = stock_price %>%　filter( grepl( law , conditions) & 第二款漲跌 == vol & 市場別 == market )
        }
      if (law != "第二款")
        if(vol == "NA"){
          tmp = stock_price %>%　filter( grepl( law , conditions) & 市場別 == market )
        }
      if(vol != "NA"){
        tmp = stock_price %>%　filter( grepl( law , conditions) & 六日漲跌 == vol & 市場別 == market )
      }
      sheet = data.table()
      for(i in 17:38){ #算風險指標 第0日報酬~第10日報酬
        a.vector = tmp[[i]] 
        cat(law,colnames(tmp[,i]), vol = vol )
        
        cc = descriptive.stat.func(a.vector)
        
        sheet = rbind(sheet , cc )
      }
      vol.sheet = rbind( vol.sheet , sheet)  
    }
    big_sheet = rbind( big_sheet , vol.sheet)
  }
  #big_sheet = big_sheet %>% t()
  #colnames(big_sheet) = dic_dispose
  #big_sheet = big_sheet %>% round(4)
  
  #在這邊加入factor排序
  big_sheet = big_sheet %>% mutate( conditions = factor( conditions , order = T , levels = dic ),
                                    index = factor( index ,  order = T , levels = c(AR.return.order,CAR.return.order)  ),
                                    vol = factor( vol ,  order = T , levels = c( vol.order )  )
                                             
  )
  return(big_sheet)
}

TSE_notice_stat_sheet = each_notice_daily_func(stock_price , market = "TSE")
OTC_notice_stat_sheet = each_notice_daily_func(stock_price , market = "OTC")

AR_TSE.notice.stat = TSE_notice_stat_sheet %>% filter( !grepl("持有" , index) )
CAR_TSE.notice.stat = TSE_notice_stat_sheet %>% filter( grepl("持有" , index) )

AR_OTC.notice.stat = OTC_notice_stat_sheet %>% filter( !grepl("持有" , index) )
CAR_OTC.notice.stat = OTC_notice_stat_sheet %>% filter( grepl("持有" , index) )


write.csv(AR_TSE.notice.stat , "./tidy_up_data/注意股結果/TSE_注意股AR敘述統計表.csv" )
write.csv(CAR_TSE.notice.stat , "./tidy_up_data/注意股結果/TSE_注意股CAR敘述統計表.csv" )

write.csv(AR_OTC.notice.stat , "./tidy_up_data/注意股結果/OTC_注意股AR敘述統計表.csv"  )
write.csv(CAR_OTC.notice.stat , "./tidy_up_data/注意股結果/OTC_注意股CAR敘述統計表.csv"  )

###表格轉置:練習###
#sheet = CAR_OTC.notice.stat
#market= "OTC"
AR.CAR_func = function(sheet , market ){
  holysheet = sheet[,c(1:5)]
  holysheet = holysheet %>% mutate( vol = ifelse( vol == "NA" , "不分類" , vol    ) ,
                                              vol = ifelse( vol == "上漲" , "漲幅" , vol    ) 
                                          ) %>%
                                    mutate( vol = ifelse( vol == "2" , "漲幅" , vol    ) ,
                                            vol = ifelse( vol == "3" , "跌幅" , vol    ) 
                                          )
  holysheet = holysheet %>% unite(.,"name",c("market","conditions","vol"),sep="_")
 
  #####
  #其實應該用上面那個檔案拆開來計算，但實在是太麻煩了，文字排序有夠累
  dic.no.tidy = paste(  dic,"不分類" ,sep = "_"  )
  dic.up = paste( dic ,"漲幅" ,sep = "_"  )
  dic.down = paste(  dic ,"跌幅" ,sep = "_"  ) 
  dic.vol.order = c(dic.no.tidy , dic.up , dic.down )
  rm(dic.no.tidy,dic.up ,dic.down  )
  dic.vol.order = paste( toupper(market) , dic.vol.order ,sep = "_"  ) 
  
  
  holysheet$name = factor(  holysheet$name ,  order = T , levels = c(dic.vol.order)  )   
  holysheet = holysheet %>% group_by(name) %>% filter(! duplicated(index) ) %>% ungroup
  holysheet = spread(holysheet , name , mean ,drop = T)
  
  #holysheet = holysheet[,c("index",dic.vol.order),with = F]
  
  return(holysheet)
}

AR_TSE.notice = AR.CAR_func(AR_TSE.notice.stat , market = "TSE"  )
CAR_TSE.notice = AR.CAR_func(CAR_TSE.notice.stat , market = "TSE"  )

AR_OTC.notice = AR.CAR_func(AR_OTC.notice.stat , market = "OTC"  ) #OTC沒有第八款跟第十四款的跌幅
CAR_OTC.notice = AR.CAR_func(CAR_OTC.notice.stat , market = "OTC"  )

write.csv(AR_TSE.notice , "./tidy_up_data/注意股結果/TSE_注意股AR報酬表.csv" , row.names  = F)
write.csv(CAR_TSE.notice , "./tidy_up_data/注意股結果/TSE_注意股CAR報酬表.csv" ,row.names  = F )

write.csv(AR_OTC.notice , "./tidy_up_data/注意股結果/OTC_注意股AR報酬表.csv" ,row.names  = F )
write.csv(CAR_OTC.notice , "./tidy_up_data/注意股結果/OTC_注意股CAR報酬表.csv" ,row.names  = F )


# unique(holysheet$name) == dic.vol.order
# unique(holysheet$name) %>% length



#算第每款不分類持有報酬，TSE
# each.law.return.func = function(stock_price , market , volatility){
#   no_tidy_sheet = data.table()
#   for (law in dic){
#   #第二款條款
#     if(law == "第二款"){
#       #test cc = stock_price %>% filter( conditions == "第二款" )
#       if(volatility == "NA"){
#         each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) )  }
#       if(volatility != "NA"){
#         each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) & 第二款漲跌 == volatility )}
#     }
#     if(law != "第二款"){
#       if(volatility == "NA"){
#         each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) )  }
#       if(volatility != "NA"){
#         each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) & 六日漲跌 == volatility )}
#     }  
#     sheet = data.table()
#     for (i in 16:35){ #有修改的話位置要調
#       cc = data.table(mean = mean(each_law[[i]] , na.rm=T ) )
#       colnames(cc) = colnames(each_law[,i])
#       sheet = cbind(sheet , cc )
#     }
#     no_tidy_sheet = rbind( no_tidy_sheet , sheet)
#   }
#   no_tidy_sheet = no_tidy_sheet %>% t()
#   colnames(no_tidy_sheet) = dic
#   no_tidy_sheet = no_tidy_sheet %>% round(4)
#   return(no_tidy_sheet)
# }
# 
# #提前持有計算
# early_hold.law.return.func = function(stock_price , market , volatility){
#   no_tidy_sheet = data.table()
#   for (law in dic){
#     #第二款條款
#     if(law == "第二款"){
#       #test cc = stock_price %>% filter( conditions == "第二款" )
#       if(volatility == "NA"){
#         each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) )  }
#       if(volatility != "NA"){
#         each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) & 第二款漲跌 == volatility )}
#     }
#     if(law != "第二款"){
#       if(volatility == "NA"){
#         each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) )  }
#       if(volatility != "NA"){
#         each_law = stock_price %>% filter( grepl( law , conditions) & 市場別 == (market %>% as.character) & 六日漲跌 == volatility )}
#     }  
#     sheet = data.table()
#     for (i in 36:56){ #有修改的話位置要調
#       cc = data.table(mean = mean(each_law[[i]] , na.rm=T ) )
#       colnames(cc) = colnames(each_law[,i])
#       sheet = cbind(sheet , cc )
#     }
#     no_tidy_sheet = rbind( no_tidy_sheet , sheet)
#   }
#   no_tidy_sheet = no_tidy_sheet %>% t()
#   colnames(no_tidy_sheet) = dic
#   no_tidy_sheet = no_tidy_sheet %>% round(4)
#   return(no_tidy_sheet)
# }
# 
# #不分漲跌
# TSE_no_catagory = each.law.return.func(stock_price , market = "TSE" ,volatility = "NA" )
# OTC_no_catagory = each.law.return.func(stock_price , market = "OTC" ,volatility = "NA" )
# 
# #分漲跌 #其實近六日漲跌幅只適用 1,3,4,5,7,11 款，其他的要想辦法刪掉
# TSE_vol_upside = each.law.return.func(stock_price , market = "TSE" ,volatility = "上漲")
# TSE_vol_downside = each.law.return.func(stock_price , market = "TSE" ,volatility = "下跌") 
# #分漲跌 #
# OTC_vol_upside = each.law.return.func(stock_price , market = "OTC" ,volatility = "上漲")
# OTC_vol_downside = each.law.return.func(stock_price , market = "OTC" ,volatility = "下跌")
# 
# 
# #存檔的部分
# write.csv(TSE_no_catagory , "./tidy_up_data/注意股結果/不分類TSE持有報酬.csv" )
# write.csv(OTC_no_catagory , "./tidy_up_data/注意股結果/不分類OTC持有報酬.csv"  )
# 
# write.csv(TSE_vol_upside , "./tidy_up_data/注意股結果/漲幅TSE持有報酬.csv"  )
# write.csv(TSE_vol_downside , "./tidy_up_data/注意股結果/跌幅TSE持有報酬.csv" )
# 
# write.csv(OTC_vol_upside , "./tidy_up_data/注意股結果/漲幅OTC持有報酬.csv"  )
# write.csv(OTC_vol_downside , "./tidy_up_data/注意股結果/跌幅OTC持有報酬.csv"  )
# 

#我想想漲跌幅要分的更金確去意點
#分漲跌幅條款好了

#####提前持有
#不分漲跌
# early_TSE_no_catagory = early_hold.law.return.func(stock_price , market = "TSE" ,volatility = "NA" )
# early_OTC_no_catagory = early_hold.law.return.func(stock_price , market = "OTC" ,volatility = "NA" )
# 
# #分漲跌 #其實近六日漲跌幅只適用 1,3,4,5,7,11 款，其他的要想辦法刪掉
# early_TSE_vol_upside = early_hold.law.return.func(stock_price , market = "TSE" ,volatility = "上漲")
# early_TSE_vol_downside = early_hold.law.return.func(stock_price , market = "TSE" ,volatility = "下跌") 
# #分漲跌 #
# early_OTC_vol_upside = early_hold.law.return.func(stock_price , market = "OTC" ,volatility = "上漲")
# early_OTC_vol_downside = early_hold.law.return.func(stock_price , market = "OTC" ,volatility = "下跌")

#存檔的部分
# write.csv(early_TSE_no_catagory , "./tidy_up_data/提前持有_不分類TSE持有報酬.csv" , row.names = T , col.names = T )
# write.csv(early_OTC_no_catagory , "./tidy_up_data/提前持有_不分類OTC持有報酬.csv" , row.names = T , col.names = T )
# 
# write.csv(early_TSE_vol_upside , "./tidy_up_data/提前持有_漲幅TSE持有報酬.csv" , row.names = T , col.names = T )
# write.csv(early_TSE_vol_downside , "./tidy_up_data/提前持有_跌幅TSE持有報酬.csv" , row.names = T , col.names = T )
# 
# write.csv(early_OTC_vol_upside , "./tidy_up_data/提前持有_漲幅OTC持有報酬.csv" , row.names = T , col.names = T )
# write.csv(early_OTC_vol_downside , "./tidy_up_data/提前持有_跌幅OTC持有報酬.csv" , row.names = T , col.names = T )


#極度需要漲跌樣本數，順便算一下勝率(ok 明天來算一下處置股)
all.sample.sheet = data.table()
for (market in c("TSE","OTC")){
  for (law in dic){
    for(vol in c("上漲","下跌","x")){
      if(vol == "x"){
        n_count = nrow(stock_price %>% filter(  grepl( law , conditions) & 市場別 == market ))
      }
      if(vol %chin% c("上漲","下跌")){
        n_count = nrow(stock_price %>% filter(  grepl( law , conditions) & 市場別 == market &  六日漲跌 == vol ))
      }
      sheet = data.table(條款 = law ,市場別 = market , 漲跌 = vol , 樣本數 = n_count )  
      all.sample.sheet = rbind(all.sample.sheet , sheet)
}}}
vol.sample.sheet = all.sample.sheet %>% filter(漲跌 != "x") %>% group_by(條款,市場別) %>%　mutate(漲跌比 = (樣本數/sum(樣本數)) %>% round(4)) %>%
                  　arrange(漲跌,市場別)

all.sample.sheet = all.sample.sheet %>% filter(漲跌 == "x") %>% arrange(漲跌,市場別)
  

write.csv(all.sample.sheet , "./tidy_up_data/注意股結果/注意股全部市場漲跌樣本數.csv" , row.names = F )
write.csv(vol.sample.sheet , "./tidy_up_data/注意股結果/注意股各市場漲跌樣本數.csv" , row.names = F )


##### 下面這邊是條款分類的部分 #####
table(stock_price$條款分類)
com_dic = c("漲跌組","成交量組","成交量漲跌組","其他")
stock_price$條款分類 = factor(  stock_price$條款分類 ,  order = T , levels = com_dic  )

catR_notice_func = function(stock_price ){   #market = "TSE"
  com_dic = c("漲跌組","成交量組","成交量漲跌組","其他")
  vol.list = c("NA","上漲","下跌")
  big_sheet = data.table()
  for(law in com_dic){
    vol.sheet = data.table()
    for(vol in vol.list){ #篩選出股票順便更具第二款分類
        if(vol == "NA"){
          tmp = stock_price %>%　filter( grepl( law , 條款分類) ) #& 市場別 == market )
        }
        if(vol != "NA"){
          tmp = stock_price %>%　filter( grepl( law , 條款分類) & 六日漲跌 == vol )#& 市場別 == market )
      }
      sheet = data.table()
      for(i in 17:38){ #算風險指標 第0日報酬~第10日報酬
        a.vector = tmp[[i]] 
        cat(law,colnames(tmp[,i]),vol = vol)
        if(length(a.vector) == 0){cc = data.table(conditions = law,
                                                  index = colnames(tmp[,i]) , 
                                                  market = market,
                                                  vol = vol,
                                                  mean = NA  , sd =NA  , max = NA ,
                                                  min = NA , quantile_75 =NA, quantile_25=NA,  
                                                  win.rate = NA,sample = length(a.vector), p.value = NA )
        }
        
        if(length(a.vector) > 1){
          cc = data.table(conditions = law,
                          index = colnames(tmp[,i]) , 
                          #market = market,
                          vol = vol,
                          mean = mean(a.vector , na.rm=T ) %>% round(4) , 
                          sd = sd(a.vector , na.rm=T) %>% round(4) ,
                          max = max(a.vector , na.rm=T ) %>% round(4) ,
                          quantile_75 = quantile(a.vector , probs = 0.75 , na.rm=T ) %>% round(4),
                          median = quantile(a.vector , probs = 0.5 , na.rm=T ) %>% round(4),
                          quantile_25 = quantile(a.vector , probs = 0.25 , na.rm=T ) %>% round(4),
                          min = min(a.vector , na.rm=T ) %>% round(4) ,
                          win.rate =  (length( (which(a.vector >= 0))) / length(a.vector)) %>% round(2),
                          sample = length(a.vector),
                          p.value = t.test(a.vector)[[3]] %>% round(4)
          )
        }
        cc = cc %>% mutate(
          p.value.mark = ifelse( p.value < 0.1 , "*" , ""  ) ,
          p.value.mark = ifelse( p.value < 0.05 , "**" , p.value.mark  ),
          p.value.mark = ifelse( p.value < 0.01 , "***" , p.value.mark  )
        )
        
        sheet = rbind(sheet , cc )
      }
      vol.sheet = rbind( vol.sheet , sheet)  
    }
    big_sheet = rbind( big_sheet , vol.sheet)
  }
  #big_sheet = big_sheet %>% t()
  #colnames(big_sheet) = dic_dispose
  #big_sheet = big_sheet %>% round(4)
  
  #在這邊加入factor排序
  big_sheet = big_sheet %>% mutate( conditions = factor( conditions , order = T , levels = com_dic ),
                                    index = factor( index ,  order = T , levels = c(AR.return.order,CAR.return.order)  ),
                                    vol = factor( vol ,  order = T , levels = c( vol.order )  )
                                    
  )
  return(big_sheet)
}

notice.Return.stat.all = catR_notice_func(stock_price)

AR_notice.Return.stat.all = notice.Return.stat.all %>% filter( !grepl("持有" , index) )
CAR_notice.Return.stat.all = notice.Return.stat.all %>% filter( grepl("持有" , index) )

write.csv(AR_notice.Return.stat.all , "./tidy_up_data/注意股結果/全市場_注意股AR報酬表.csv" , row.names  = F)
write.csv(CAR_notice.Return.stat.all , "./tidy_up_data/注意股結果/全市場_注意股CAR報酬表.csv" ,row.names  = F )


#極度需要漲跌樣本數，順便算一下勝率(ok 明天來算一下處置股)
all.sample.sheet = data.table()
for (market in c("TSE","OTC")){
  for (law in com_dic){
    for(vol in c("上漲","下跌","x")){
      if(vol == "x"){
        n_count = nrow(stock_price %>% filter(  grepl( law , 條款分類) & 市場別 == market ))
      }
      if(vol %chin% c("上漲","下跌")){
        n_count = nrow(stock_price %>% filter(  grepl( law , 條款分類) & 市場別 == market &  六日漲跌 == vol ))
      }
      sheet = data.table(條款 = law ,市場別 = market , 漲跌 = vol , 樣本數 = n_count )  
      all.sample.sheet = rbind(all.sample.sheet , sheet)
    }}}
com.vol.sample.sheet = all.sample.sheet %>% filter(漲跌 != "x") %>% group_by(條款,市場別) %>%　mutate(漲跌比 = (樣本數/sum(樣本數)) %>% round(4)) %>%
  arrange(漲跌,市場別)

com.all.sample.sheet = all.sample.sheet %>% filter(漲跌 == "x") %>% arrange(漲跌,市場別)





write.csv(com.vol.sample.sheet , "./tidy_up_data/注意股結果/條款分類_注意股全部市場漲跌樣本數.csv" , row.names = F )
write.csv(com.all.sample.sheet , "./tidy_up_data/注意股結果/條款分類_注意股各市場漲跌樣本數.csv" , row.names = F )




##### 不分類條款報酬率 #####

noclassify_notice_func = function(stock_price){   
  
  dic = c("第一款","第二款","第三款","第四款","第五款","第六款","第七款","第八款",
          "第九款","第十款","第十一款","第十二款","第十三款","第十四款" )
  vol.list = c("NA","上漲","下跌")
  big_sheet = data.table()
  for(law in dic){
    vol.sheet = data.table()
    for(vol in vol.list){ #篩選出股票順便更具第二款分類
      if (law == "第二款")
        if(vol == "NA"){
          tmp = stock_price %>%　filter( grepl( law , conditions)  )
        }
      if(vol != "NA"){
        tmp = stock_price %>%　filter( grepl( law , conditions) & 第二款漲跌 == vol )
      }
      if (law != "第二款")
        if(vol == "NA"){
          tmp = stock_price %>%　filter( grepl( law , conditions)  )
        }
      if(vol != "NA"){
        tmp = stock_price %>%　filter( grepl( law , conditions) & 六日漲跌 == vol  )
      }
      sheet = data.table()
      for(i in 17:38){ #算風險指標 第0日報酬~第10日報酬
        a.vector = tmp[[i]] 
        cat(law,colnames(tmp[,i]),vol = vol)
        if(length(a.vector) == 0){cc = data.table(conditions = law,
                                                  index = colnames(tmp[,i]) , 
                                                  #market = market,
                                                  vol = vol,
                                                  mean = NA  , sd =NA  , max = NA ,
                                                  quantile_75 =NA, median = NA ,quantile_25 = NA,  min = NA ,
                                                  win.rate = NA,sample = length(a.vector), p.value = NA )
        }
        
        if(length(a.vector) > 1){
          cc = data.table(conditions = law,
                          index = colnames(tmp[,i]) , 
                         # market = market,
                          vol = vol,
                          mean = mean(a.vector , na.rm=T ) %>% round(4) , 
                          sd = sd(a.vector , na.rm=T) %>% round(4) ,
                          max = max(a.vector , na.rm=T ) %>% round(4) ,
                          
                          quantile_75 = quantile(a.vector , probs = 0.75 , na.rm=T ) %>% round(4),
                          median = quantile(a.vector , probs = 0.5 , na.rm=T ) %>% round(4),
                          quantile_25 = quantile(a.vector , probs = 0.25 , na.rm=T ) %>% round(4), 
                          min = min(a.vector , na.rm=T ) %>% round(4) ,
                          win.rate =  (length( (which(a.vector >= 0))) / length(a.vector)) %>% round(2),
                          sample = length(a.vector),
                          p.value = t.test(a.vector)[[3]] %>% round(4)
          )
        }
        cc = cc %>% mutate(
          p.value.mark = ifelse( p.value < 0.1 , "*" , ""  ) ,
          p.value.mark = ifelse( p.value < 0.05 , "**" , p.value.mark  ),
          p.value.mark = ifelse( p.value < 0.01 , "***" , p.value.mark  )
        )
        
        sheet = rbind(sheet , cc )
      }
      vol.sheet = rbind( vol.sheet , sheet)  
    }
    big_sheet = rbind( big_sheet , vol.sheet)
  }
  #big_sheet = big_sheet %>% t()
  #colnames(big_sheet) = dic_dispose
  #big_sheet = big_sheet %>% round(4)
  
  #在這邊加入factor排序
  big_sheet = big_sheet %>% mutate( conditions = factor( conditions , order = T , levels = dic ),
                                    index = factor( index ,  order = T , levels = c(AR.return.order,CAR.return.order)  ),
                                    vol = factor( vol ,  order = T , levels = c( vol.order )  )
                                    
  )
  return(big_sheet)
}

noclassify_notice = noclassify_notice_func(stock_price)

AR.noclassify_notice = noclassify_notice %>% filter( !grepl("持有" , index) )
CAR.noclassify_notice = notice.Return.stat.all %>% filter( grepl("持有" , index) )

write.csv(AR.noclassify_notice , "./tidy_up_data/注意股結果/全市場_注意股AR報酬表.csv" , row.names  = F)
write.csv(CAR.noclassify_notice , "./tidy_up_data/注意股結果/全市場_注意股CAR報酬表.csv" ,row.names  = F )


