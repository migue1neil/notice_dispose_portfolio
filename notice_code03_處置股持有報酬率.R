# 要做策略的話，可以生成一個快達成條件的欄位，例如近30個交易日，要12天才被列處置，先偵測11日

#先看看報酬
#第一次處置，漲
#第一次處置，跌
#第二次處置，漲
#第二次處置，跌
 
setwd("C:/Users/Neil/Documents/git-repos/notice_stock_portfolio")

library(data.table)
library(tidyverse)
library(lubridate)
library(zoo)

dic = c("第一款","第二款","第三款","第四款","第五款","第六款","第七款","第八款",
        "第九款","第十款","第十一款","第十二款","第十三款","第十四款" )

dic_dispose = c("第一次處置","第二次處置")


stock_price = fread("./tidy_up_data/合併後股價資料表_20110101_20221018.csv" , header = T , sep = "," ,
                    colClasses = list(character = c(3,4,37:39,43:44) , numeric= c(1,2,5:36,40:42) ))

stock_price = stock_price[,c(1:5,8:13,15,24,26,37:49)]

test_list = stock_price %>% filter( is.na(處置次數)== F)　%>%　select(證券代碼) %>% unique  #只選出有處置的證券代碼
stock_price = stock_price[證券代碼 %in% test_list[[1]],]
#rm(stock_price_test)
rm(test_list)
cc =  stock_price %>% filter(證券代碼 == "2603" & 年月日 %between% c(20200101,20221231) )

####區分處置股漲跌幅原因

#一樣先算出漲跌幅報酬

stock_price = stock_price %>% group_by(證券代碼) %>%
  mutate( 六日漲跌 = ifelse((調整收盤價 -lag(調整收盤價 ,5)) / lag(調整收盤價 , 5) >= 0 , "+", "-" )) %>% ungroup

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

#####向量方式算出每個時間點的日報酬#####
for (ndays in 0:10){
  if (ndays == 0){ stock_price = stock_price %>% group_by(證券代碼) %>% 
    mutate( 第0日報酬 = (調整收盤價-調整開盤價)/調整開盤價) } %>% ungroup #持有0日是偷看
  if(ndays != 0){
    stock_price = stock_price %>% group_by(證券代碼) %>% 
      mutate( holding_ndays = ( lead(調整收盤價, n = ndays) - lead(調整開盤價, n = ndays) )/lead(調整開盤價, n = ndays)) %>% ungroup
    colnames(stock_price)[NCOL(stock_price)]  = paste0("第",ndays,"日報酬")
  }
}
# stock_price[,c(NCOL(stock_price)-20 :NCOL(stock_price))] = 
#   stock_price[,c(NCOL(stock_price)-20:NCOL(stock_price))] %>% round(digits = 4) #把全部取小數點後四位

###先用向量方式算出每個時間點持有t0~t20的報酬
for (ndays in 1:10){
  if (ndays == 0){ stock_price = stock_price %>% group_by(證券代碼) %>% 
    mutate( 持有0日報酬 = (調整收盤價-調整開盤價)/調整開盤價) } %>% ungroup #持有0日是偷看
  if(ndays != 0){
    stock_price = stock_price %>% group_by(證券代碼) %>% 
      mutate( holding_ndays = ( lead(調整收盤價, n = ndays) - lead(調整開盤價, n = 1 ) )/lead(調整開盤價, n = 1)) %>% ungroup
    colnames(stock_price)[NCOL(stock_price)]  = paste0("持有",ndays,"日報酬")
  }
}
# stock_price[,c(NCOL(stock_price)-21 :NCOL(stock_price))] = 
#   stock_price[,c(NCOL(stock_price)-21:NCOL(stock_price))] %>% round(digits = 4 , na.rm = T) #把全部取小數點後四位



####我現在要算一個每天日報酬風險的表格#####
#計算每組的平均報酬，風險指標，如何跑顯著性???
each_dispose_daily_func = function(stock_price , market){  
  dic_dispose = c("第一次處置","第二次處置")
  vol.list = c("NA","+","-")
  big_sheet = data.table()
  # market = "OTC"
  for(dic in dic_dispose){
    vol.sheet = data.table()
    for(vol in vol.list){
      if(vol == "NA"){
        tmp = stock_price %>%　filter( grepl( dic , 處置次數) & 市場別 == market )
      }
      if(vol != "NA"){
        tmp = stock_price %>%　filter( grepl( dic , 處置次數) & 六日漲跌 == vol & 市場別 == market )
      }
      sheet = data.table()
      for(i in 34:44){ #算風險指標 第0日報酬~第10日報酬
        a.vector = tmp[[i]] 
        cc = data.table(disposed = dic,
                        index = colnames(tmp[,i]) , 
                        market = market,
                        vol = vol,
                        mean = mean(a.vector , na.rm=T ) %>% round(4) , 
                        sd = sd(a.vector , na.rm=T) %>% round(4) ,
                        max = max(a.vector , na.rm=T ) %>% round(4) ,
                        min = min(a.vector , na.rm=T ) %>% round(4) ,
                        quantile_75 = quantile(a.vector , probs = 0.75 , na.rm=T ) %>% round(4),
                        quantile_25 = quantile(a.vector , probs = 0.25 , na.rm=T ) %>% round(4),  
                        win.rate =  (length( (which(a.vector >= 0))) / length(a.vector)) %>% round(2),
                        sample = length(a.vector),
                        p.value = t.test(a.vector)[[3]] %>% round(4)
                          )
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
  return(big_sheet)
}

TSE_stat_sheet = each_dispose_daily_func(stock_price , market = "TSE")

OTC_stat_sheet = each_dispose_daily_func(stock_price , market = "OTC")

write.csv(TSE_stat_sheet , "./tidy_up_data/處置股結果/TSE_處置股敘述統計表.csv" , row.names = F )
write.csv(OTC_stat_sheet , "./tidy_up_data/處置股結果/OTC_處置股敘述統計表.csv" , row.names = F )



###
each_dispose_func = function(stock_price , market , vol = "NA"  ){  #計算每組的平均報酬
  dic_dispose = c("第一次處置","第二次處置")
  big_sheet = data.table()
  for(dic in dic_dispose){
    if(vol == "NA"){
      tmp = stock_price %>%　filter( grepl( dic , 處置次數) & 市場別 == market )
    }
    if(vol != "NA"){
      tmp = stock_price %>%　filter( grepl( dic , 處置次數) & 六日漲跌 == vol & 市場別 == market )
    }
    sheet = data.table()
    for(i in 34:43){ 
      cc = data.table(mean = mean(tmp[[i]] , na.rm=T ) )
      colnames(cc) = colnames(tmp[,i])
      sheet = cbind(sheet , cc )
    }
    big_sheet = rbind( big_sheet , sheet)  
  }
  big_sheet = big_sheet %>% t()
  colnames(big_sheet) = dic_dispose
  big_sheet = big_sheet %>% round(4)
  return(big_sheet)
}


dispose.TSE = each_dispose_func(stock_price , market = "TSE" , vol = "NA" )
dispose.OTC = each_dispose_func(stock_price , market = "OTC" , vol = "NA" )

dispose.TSE.upside = each_dispose_func(stock_price , market = "TSE" , vol = "+" )
dispose.TSE.downside = each_dispose_func(stock_price , market = "TSE" , vol = "-" )

dispose.OTC.upside = each_dispose_func(stock_price , market = "OTC" , vol = "+" )
dispose.OTC.downside = each_dispose_func(stock_price , market = "OTC" , vol = "-" )


write.csv(dispose.TSE , "./tidy_up_data/處置股結果/TSE處置持有報酬_不分類.csv" , row.names = T , col.names = T )
write.csv(dispose.TSE.upside , "./tidy_up_data/處置股結果/TSE處置持有報酬_漲幅.csv" , row.names = T , col.names = T )
write.csv(dispose.TSE.downside , "./tidy_up_data/處置股結果/TSE處置持有報酬_跌幅.csv" , row.names = T , col.names = T )

write.csv(dispose.OTC , "./tidy_up_data/處置股結果/OTC處置持有報酬_不分類.csv" , row.names = T , col.names = T )
write.csv(dispose.OTC.upside , "./tidy_up_data/處置股結果/OTC處置持有報酬_漲幅.csv" , row.names = T , col.names = T )
write.csv(dispose.OTC.downside , "./tidy_up_data/處置股結果/OTC處置持有報酬_跌幅.csv" , row.names = T , col.names = T )


#極度需要漲跌樣本數，順便算一下勝率(ok 明天來算一下處置股)
dispose.sample.sheet = data.table()
for (market in c("TSE","OTC")){
  for (law in dic_dispose){
    for(vol in c("+","-","x")){
      if(vol == "x"){
        n_count = nrow(stock_price %>% filter(  grepl( law , 處置次數) & 市場別 == market ))
      }
      if(vol %chin% c("+","-")){
        n_count = nrow(stock_price %>% filter(  grepl( law , 處置次數) & 市場別 == market &  六日漲跌 == vol ))
      }
      sheet = data.table(處置次數 = law ,市場別 = market , 漲跌 = vol , 樣本數 = n_count )  
      dispose.sample.sheet = rbind(dispose.sample.sheet , sheet)
    }}}
vol.sample.sheet = dispose.sample.sheet %>% filter(漲跌 != "x") %>% group_by(處置次數,市場別) %>%　mutate(漲跌比 = (樣本數/sum(樣本數)) %>% round(4)) %>%
  arrange(市場別,處置次數)

dispose.sample.sheet = dispose.sample.sheet %>% filter(漲跌 == "x")

write.csv(dispose.sample.sheet , "./tidy_up_data/處置股結果/處置股全部市場漲跌樣本數.csv" , row.names = T )
write.csv(vol.sample.sheet , "./tidy_up_data/處置股結果/處置股各市場漲跌樣本數.csv" , row.names = T )


#在過去十年中，因為下跌而進入第二次處置的股票樣本有136筆，其中




cc = t.test(stock_price$第0日報酬)[[3]]



?t.test


stock_price = stock_price %>% group_by(證券代碼) %>% mutate( last_notice = lag(conditions,1 ) ) %>% ungroup()

stock_price = stock_price %>% group_by(證券代碼) %>% mutate( last_2notice = lag(conditions,2 ) ) %>% ungroup()


#####現在是要把處置股做得更細膩議點 將處置股根據第一次處置，第二次處置，以及分盤時間來做區分


####改寫上面的程式碼
#第一次分盤，分成5分鐘，10分鐘和其他
#第二次分盤，分成20分鐘，25分鐘和其他

xx1u = stock_price %>% filter(處置次數 == "第一次處置" , 六日漲跌 == "+") %>% mutate( 
  分盤分組 = ifelse( 分盤 %chin% c("五分鐘","十分鐘","四十五分鐘"),分盤, "其他"  ))
table(xx1u$分盤)
table(xx1u$分盤分組)


xx1d = stock_price %>% filter(處置次數 == "第一次處置" , 六日漲跌 == "-") %>% mutate( 
  分盤分組 = ifelse( 分盤 %chin% c("五分鐘","十分鐘","四十五分鐘"),分盤, "其他"  ))
table(xx1d$分盤)
table(xx1d$分盤分組)

xx2u = stock_price %>% filter(處置次數 == "第二次處置" , 六日漲跌 == "+") %>% mutate( 
  分盤分組 = ifelse( 分盤 %chin% c("二十分鐘","二十五分鐘","六十分鐘"),分盤, "其他"  ))
table(xx2u$分盤)
table(xx2u$分盤分組)

xx2d = stock_price %>% filter(處置次數 == "第二次處置" , 六日漲跌 == "-")  %>% mutate( 
  分盤分組 = ifelse( 分盤 %chin% c("二十分鐘","二十五分鐘","六十分鐘"),分盤, "其他"  ))
table(xx2d$分盤)
table(xx2d$分盤分組)





stock_price = stock_price %>% mutate(
   分組分盤_1 = ifelse( 分盤 %chin% c("五分鐘","十分鐘","四十五分鐘"),分盤, "其他"  ),
   分組分盤_2 = ifelse( 分盤 %chin% c("二十分鐘","二十五分鐘","六十分鐘"),分盤, "其他"  )
  ) %>% mutate(
    分組分盤_1 = ifelse( is.na(分盤) == T , NA  , 分組分盤_1  ),
    分組分盤_2 = ifelse( is.na(分盤) == T , NA  , 分組分盤_2  )
  )

#統計出現次數 #做表格好煩喔先不管她
cc = stock_price %>% filter(處置次數 == "第一次處置")
first_dispose_sample = data.table( 處置次數 = "第一次處置" , table(cc$分盤) )
cc_2 = stock_price %>% filter(處置次數 == "第二次處置")
second_dispose_sample = data.table( 處置次數 = "第二次處置" , table(cc_2$分盤) )
stat.sheet3 = rbind(first_dispose_sample , second_dispose_sample) %>%
  arrange( 處置次數 , -N)
#cc_3 = rbind(table(cc$分組分盤_1) , table(cc_2$分組分盤_2))
stat.sheet4 = data.table(處置次數 = c("第一次處置","第二次處置") , cc_3  )


table(cc_2$分組分盤_2)


##### 複製上面的程式碼，再加入分盤資料
handled.dispose = function(stock_price , market ){ 
  #要分組的東西
  dic_dispose = c("第一次處置","第二次處置")
  fir.dp = c("五分鐘","十分鐘","四十五分鐘","其他")
  sec.dp = c("二十分鐘","二十五分鐘","六十分鐘","其他")
  vol.list = c("NA","+","-")
  
  big_sheet = data.table()
  for(dic in dic_dispose){
    # dic = "第二次處置"
    # market = "TSE"
    # i = 2
    medium.sheet = data.table()
    for( i in c(1:4) ){ #這個是設計來給分盤分組使用的
        
        vol.sheet = data.table()
        for(vol in vol.list){
          
          #基本篩選 
          tmp = stock_price %>% filter( grepl( dic , 處置次數)  & 市場別 == market )
         #分盤分組
          if(dic == "第一次處置"){dp.group = fir.dp}
          if(dic == "第二次處置"){dp.group = sec.dp}
          tmp = tmp %>% mutate(分盤分組 = ifelse( 分盤 %chin% dp.group ,分盤, "其他") ) %>% #上面有篩出處置的股票，所以不用再排除一般股票一次
                        filter(分盤分組 == dp.group[i] )
          #漲跌篩選
          if(vol != "NA"){ 
            tmp = tmp %>% filter(六日漲跌 == vol)
          }
          stat_func = function(tmp  , disposed = dic , handled.time = dp.group[i]  ){
            cat(disposed, handled.time ,vol , "\n", sep = "_")
            #handled.time = handled.time
            sheet = data.table()
            for(i in 34:44){ #算風險指標 第0日報酬~第10日報酬
              a.vector = tmp[[i]] 
              
              if(length(a.vector) <= 1){cc = data.table(disposed = disposed,
                                                        分盤時間 = handled.time,
                                                        index = colnames(tmp[,i]) , 
                                                        market = market,
                                                        vol = vol,
                                                        mean = NA  , sd =NA  , max = NA ,
                                                        min = NA , quantile_75 =NA, quantile_25=NA,  
                                                        winrate = NA,sample = length(a.vector), p.value = NA )
              }
              if(length(a.vector) > 1){cc = data.table(disposed = disposed,
                                        分盤時間 = handled.time ,
                                        index = colnames(tmp[,i]) , 
                                        market = market,
                                        vol = vol,
                                        mean = mean(a.vector , na.rm=T ) %>% round(4) , 
                                        sd = sd(a.vector , na.rm=T) %>% round(4) ,
                                        max = max(a.vector , na.rm=T ) %>% round(4) ,
                                        min = min(a.vector , na.rm=T ) %>% round(4) ,
                                        quantile_75 = quantile(a.vector , probs = 0.75 , na.rm=T ) %>% round(4),
                                        quantile_25 = quantile(a.vector , probs = 0.25 , na.rm=T ) %>% round(4),  
                                        winrate =  (length( (which(a.vector >= 0))) / length(a.vector)) %>% round(2),
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
            return(sheet)
          }
          sheet = stat_func(tmp)
          vol.sheet = rbind( vol.sheet , sheet)  
        }
        medium.sheet = rbind( medium.sheet, vol.sheet  )
      }
      big_sheet = rbind( big_sheet , medium.sheet)
    }
    return(big_sheet)
  }

ccd = handled.dispose( stock_price , market = "TSE")
cce = handled.dispose( stock_price , market = "OTC")
# rm(cc)