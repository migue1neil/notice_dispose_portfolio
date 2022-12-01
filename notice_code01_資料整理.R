setwd("C:/Users/Neil/Documents/git-repos/notice_stock_portfolio")
library(tidyverse)
library(lubridate)
library(data.table)

##### 本code在處理 注意股資料/處置股資料/出現次數統計表

#筆記 : 兩個市場都有KY股，X但DR存託憑證只有TSE有OTC沒有，錯，媽的都有
#有上市轉上櫃，也有上櫃轉上市
#TEJ 1325恆大打錯了啦
#F-再生 跟再生KY是同一個東西，
#目前小結，用證券代碼合併問題應該會問題小很多。
##### 

dic = c("第一款","第二款","第三款","第四款","第五款","第六款","第七款","第八款",
        "第九款","第十款","第十一款","第十二款","第十三款","第十四款" )

dic_group = c("漲跌組","成交量組","成交量漲跌組","其他")

dic_dispose = c("第一次處置","第二次處置")

#####載入上市注意股#####
TSE_notice = fread("./orign_data/上市注意股_20110101_20221018.csv" , encoding = "UTF-8")

#調整欄位名稱
TSE_notice = TSE_notice %>% rename(c("證券代碼"="證券代號","公司名稱"="證券名稱")) 

#整理時間
TSE_notice = TSE_notice %>% mutate(年 = substr(日期,1,3) %>% as.numeric + 1911 ) %>% 
                            mutate(年月日 = paste0(年,substr(日期,5,6),substr(日期,8,9))) %>%
                            select(-日期) 

TSE_notice$年月日 = TSE_notice$年月日 %>% as.numeric

#排除非上市公司
TSE_notice = TSE_notice %>% mutate(證券代碼 = 證券代碼 %>% as.numeric) %>% na.omit %>%
             filter( substr(證券代碼,1,2) == "91" | 證券代碼 %between% c(1000,9999) ) %>%
             mutate(市場別 = "TSE") #新增市場別

#整理本益比格式
#本資料剛好沒有本益比是負值，所以可以直接這樣刪，上櫃公司有負值，需要排除 (不可這樣刪)
TSE_notice$本益比 = str_trim(TSE_notice$本益比 , side='both')
TSE_notice$本益比 = gsub("-----","",TSE_notice$本益比) 
TSE_notice$本益比 = TSE_notice$本益比 %>% as.numeric
#TSE_notice = TSE_notice[is.na(本益比) == F,] #排除沒有本益比的公司(非普通股) #不要使用，第六款會變NA

#提取出被限制的條款，並且新增第十四款
TSE_notice = TSE_notice %>% mutate( conditions =  NA )
for (i in 1:13){ 
  TSE_notice = TSE_notice %>% mutate( 
    conditions = ifelse(grepl(dic[i] ,注意交易資訊) , paste(conditions,dic[i], sep = ",") ,conditions ) )
} 
TSE_notice$conditions = gsub("NA,","",TSE_notice$conditions) #土法煉鋼把第一排NA刪除

TSE_notice = TSE_notice %>% mutate( conditions =  ifelse( grepl("監視業務督導會報", 注意交易資訊) , 
                                                          "第十四款" , conditions ) )

#在104/8/11以後修正法規，把以前的資料調整成現在的
#要先給的代數才不會被取代掉，導致錯誤，先替換一次在換回來
TSE_notice = TSE_notice %>% mutate( conditions = ifelse( grepl("第六款", conditions) & 年月日 < 20150811  , 
                                                         gsub("第六款","9",conditions) , conditions ),
                                    conditions = ifelse( grepl("第七款", conditions) & 年月日 < 20150811  , 
                                                         gsub("第七款","10",conditions) , conditions ),
                                    conditions = ifelse( grepl("第八款", conditions) & 年月日 < 20150811  , 
                                                         gsub("第八款","6",conditions) , conditions ),
                                    conditions = ifelse( grepl("第九款", conditions) & 年月日 < 20150811  , 
                                                         gsub("第九款","7",conditions) , conditions ),
                                    conditions = ifelse( grepl("第十款", conditions) & 年月日 < 20150811  , 
                                                         gsub("第十款","8",conditions) , conditions )
)

TSE_notice = TSE_notice %>% mutate( conditions = ifelse( grepl("6", conditions) , 
                                                         gsub("6","第六款",conditions) , conditions ),
                                    conditions = ifelse( grepl("7", conditions) , 
                                                         gsub("7","第七款",conditions) , conditions ),
                                    conditions = ifelse( grepl("8", conditions) , 
                                                         gsub("8","第八款",conditions) , conditions ),
                                    conditions = ifelse( grepl("9", conditions) , 
                                                         gsub("9","第九款",conditions) , conditions ),
                                    conditions = ifelse( grepl("10", conditions), 
                                                         gsub("10","第十款",conditions) , conditions )
)

#想要確認一下公告時間會不會是周6
#TSE_notice$week.day = wday(ymd(TSE_notice$年月日)) #取出星期，不應該有1(週六)跟7(週日)
# 會有週六(多是二月要補上班上課)

#排順序
TSE_notice = TSE_notice[,c(2:3,10,9,8,6:7,4,11,5)]  #排個順序         

##### TSE敘述統計表 #####
TSE.stat.sheet=data.table()
for (year in 2011:2022){
  sheet = data.table()
  for (i in 1:14){
    condition.name = dic[i]
    n_count = TSE_notice %>% filter(年== year & grepl( dic[i], conditions)) %>% nrow()
    tmp.sheet = data.table(n_count) 
    colnames(tmp.sheet) = c(condition.name)
    sheet = cbind(sheet,tmp.sheet)
  }
  tmp.sheet = data.table(年 = year , sheet)
  TSE.stat.sheet=rbind(TSE.stat.sheet , tmp.sheet)
}
rm(sheet,tmp.sheet)
#儲存統計表格
write.csv(TSE.stat.sheet , "./tidy_up_data/注意股結果/上市注意股出現次數表格.csv" , row.names = F )
#儲存上市注意股
write.csv(TSE_notice , "./tidy_up_data/注意股結果/上市注意股_20110101_20221018.csv" , row.names = F )

#test
#TSEtest = TSE_notice %>% filter(年 == 2013)

#####上櫃注意股#####
#載入上櫃注意股、警示股
OTC_notice = fread("./orign_data/上櫃注意股_20111001_20221019.csv" , sep = ",", header = T , encoding = "UTF-8" )
OTC_notice = OTC_notice[,c(2:7)]

#整理時間
OTC_notice = OTC_notice %>% mutate(年 = substr(公告日期,1,3) %>% as.numeric + 1911 ) %>% 
  mutate(年月日 = paste0(年,substr(公告日期,5,6),substr(公告日期,8,9)) %>% as.numeric ) %>%
  select(-公告日期) 

#修改欄位名稱
OTC_notice = OTC_notice %>% rename(c("證券代碼"="證券代號","公司名稱"="證券名稱")) 

#排除非上市公司
OTC_notice = OTC_notice %>% mutate(證券代碼 = 證券代碼 %>% as.numeric) %>% 
  filter(substr(證券代碼,1,2) == "91" | 證券代碼 %between% c(1000,9999) & is.na(證券代碼) == F  ) %>% 
  mutate(市場別 = "OTC")

#修理一下本益比
OTC_notice$本益比 = OTC_notice$本益比 %>% as.numeric

#提取出被限制的條款
OTC_notice = OTC_notice %>% mutate( conditions = NA )
for (i in 1:13){ 
  OTC_notice = OTC_notice %>% mutate( 
    conditions = ifelse(grepl(dic[i] ,注意交易資訊) , paste(conditions,dic[i], sep = ",") ,conditions ) )
} 
OTC_notice$conditions = gsub("NA,","",OTC_notice$conditions) #土法煉鋼把第一排NA刪除

#修正監督會報決議的條款
OTC_notice = OTC_notice %>% mutate( conditions =  ifelse( grepl("監視業務督導會報", 注意交易資訊) , 
                                                          "第十四款" , conditions ) )

#在108/04/10 以後修正法規，把以前的資料調整成現在的

OTC_notice = OTC_notice %>% mutate( conditions = ifelse( grepl("第六款", conditions) & 年月日 < 20190410  , 
                                                         gsub("第六款","9",conditions) , conditions ),
                                    conditions = ifelse( grepl("第七款", conditions) & 年月日 < 20190410  , 
                                                         gsub("第七款","10",conditions) , conditions ),
                                    conditions = ifelse( grepl("第八款", conditions) & 年月日 < 20190410  , 
                                                         gsub("第八款","6",conditions) , conditions ),
                                    conditions = ifelse( grepl("第九款", conditions) & 年月日 < 20190410  , 
                                                         gsub("第九款","7",conditions) , conditions ),
                                    conditions = ifelse( grepl("第十款", conditions) & 年月日 < 20190410  , 
                                                         gsub("第十款","8",conditions) , conditions )
)

OTC_notice = OTC_notice %>% mutate( conditions = ifelse( grepl("6", conditions) , 
                                                         gsub("6","第六款",conditions) , conditions ),
                                    conditions = ifelse( grepl("7", conditions) , 
                                                         gsub("7","第七款",conditions) , conditions ),
                                    conditions = ifelse( grepl("8", conditions) , 
                                                         gsub("8","第八款",conditions) , conditions ),
                                    conditions = ifelse( grepl("9", conditions) , 
                                                         gsub("9","第九款",conditions) , conditions ),
                                    conditions = ifelse( grepl("10", conditions), 
                                                         gsub("10","第十款",conditions) , conditions )
)

OTC_notice = OTC_notice[,c(1,2,8,7,6,4,5,9,3)]  #排個順序         


#####統計出現次數
table(OTC_notice$conditions)

###### OTC次數統計表 #####
otc.stat.sheet = data.table()
for (year in 2011:2022){
  sheet = data.table()
  for (i in 1:14){
    condition.name = dic[i]
    n_count = OTC_notice %>% filter(年== year & grepl( dic[i], conditions)) %>% nrow()
    tmp.sheet = data.table(n_count) 
    colnames(tmp.sheet) = c(condition.name)
    sheet = cbind(sheet,tmp.sheet)
  }
  tmp.sheet = data.table(年 = year , sheet)
  otc.stat.sheet=rbind(otc.stat.sheet , tmp.sheet)
}
rm(sheet,tmp.sheet)

#儲存統計表格
write.csv(otc.stat.sheet , "./tidy_up_data/注意股結果/上櫃注意股出現次數表格.csv" , row.names = F )

##### 上市加上櫃統計表格 #####
all.stat.sheet = TSE.stat.sheet + otc.stat.sheet
all.stat.sheet$年 = all.stat.sheet$年/2
write.csv(otc.stat.sheet , "./tidy_up_data/注意股結果/全市場注意股出現次數表格.csv" , row.names = F )

#儲存上市注意股
write.csv(OTC_notice , "./tidy_up_data/注意股結果/上櫃注意股_20110101_20221018.csv" , row.names = F )

####載入上市處置股####
TSE_dispose = fread("./orign_data/上市處置股_20110101_20221018.csv" )
TSE_dispose = TSE_dispose[,c(2:9)]

#調整年月日 因為只抓100年以後，所以可以直接這樣調整
# 年月日 == 公告日
TSE_dispose = TSE_dispose[grepl("1999", TSE_dispose$公布日期) == F] #資料西元年跟民國年放在一起有沒有搞錯啊，排除1999年的資料
TSE_dispose = TSE_dispose %>% mutate(年 = substr(公布日期,1,3) %>% as.numeric + 1911) %>%
                              mutate(年月日 = paste0(年,substr(公布日期,5,6),substr(公布日期,8,9))  %>% as.numeric) %>%
                              select(-公布日期)

#移除非普通股
TSE_dispose = TSE_dispose %>% rename(c("證券代碼"="證券代號","公司名稱"="證券名稱")) %>%
                              mutate(證券代碼 = 證券代碼 %>% as.numeric) %>% na.omit %>% 
                              filter(substr(證券代碼,1,2) == "91" | 證券代碼 %between% c(1000,9999)) %>% 
                              arrange(證券代碼, desc(年月日) )

####還沒整理完表格，需要將處置期間根據~分成兩個表格在將他們轉換成西元日期，先這樣就好，
####然後跑注意股票的CAR持有報酬，在將有上漲跟下跌的條款，股票CAR分開

TSE_dispose = separate(TSE_dispose, 處置起迄時間 , c("處置開始期間","處置結束期間"),"~")

TSE_dispose = TSE_dispose %>% mutate(處置開始期間 = paste0((substr(處置開始期間,1,3) %>% as.numeric + 1911) 
                                                    ,substr(處置開始期間,5,6),substr(處置開始期間,8,9))  %>% as.numeric ,
                                     處置結束期間 = paste0((substr(處置結束期間,1,3) %>% as.numeric + 1911) 
                                                    ,substr(處置結束期間,5,6),substr(處置結束期間,8,9)) %>% as.numeric ) %>%
                              mutate( 處置實施天數 = ymd(處置結束期間) - ymd(處置開始期間) )


###篩選出暫停多少時間####分盤交易
stoptime_func = function(dispose.sheet){
  dic_time = c("五分鐘","十分鐘","四十五分鐘","二十分鐘","二十五分鐘","六十分鐘","九十分鐘","三十分鐘","兩百七十分鐘")
  dic_time.num = c("5分鐘","10分鐘","45分鐘","20分鐘","25分鐘","60分鐘","90分鐘" ,"30分鐘" , "270分鐘") #30分鐘不知道是甚麼鬼 #270分鐘也是
  for (i in c(dic_time,dic_time.num)){
    if (i == dic_time[1]){ #目的，先生成一個變數
      dispose.sheet = dispose.sheet %>% mutate( 分盤 =  ifelse(grepl( i ,  處置內容 ) , i , NA ) )
    }else{
      dispose.sheet = dispose.sheet %>% mutate( 分盤 =  ifelse(grepl( i ,  處置內容 ) , i , 分盤 ) )}}
  for (n in 1:9){
    dispose.sheet = dispose.sheet %>% mutate( 分盤 = ifelse( 分盤 == dic_time.num[n] , dic_time[n] , 分盤 ))  %>% 
                                      filter(is.na(分盤) == F) #直接移除資料缺失，恢復交易(OTC_1筆)，僅OTC全額交割股(12筆)
    }
  dispose.sheet = dispose.sheet %>% mutate( 處置次數 = ifelse(grepl( "曾發布處置" ,  處置內容 ) , "第二次處置" , "第一次處置"  )  )
  return(dispose.sheet)
  }
#的確是有可能30分盤跟20分盤，但是是第一次處置，通常是因為督導會報決議

TSE_dispose = stoptime_func(TSE_dispose)

TSE_dispose$市場別 = "TSE" 
TSE_dispose = TSE_dispose[,c("證券代碼","公司名稱","市場別","年","年月日","處置開始期間","處置結束期間","處置次數","分盤","處置內容")]

write.csv(TSE_dispose , "./tidy_up_data/處置股結果/上市處置股_20110101_20221018.csv" , row.names = F   )

#table(TSE_dispose$分盤)
#table(TSE_dispose$處置措施)

#想要確認一下公告時間會不會是周6
#TSE_dispose$week.day = wday(ymd(TSE_dispose$年月日)) #取出星期，不應該有1(週六)跟7(週日)
#會有週六(多是二月要補上班上課)

####載入上櫃處置股####
OTC_dispose = fread("./orign_data/上櫃處置股_20111001_20221019.csv")
OTC_dispose = OTC_dispose[,c(2:7)]

#調整年月日 因為只抓100年以後，所以可以直接這樣調整
# 年月日 == 公布日期
OTC_dispose = OTC_dispose[grepl("1999", OTC_dispose$公布日期) == F] #資料西元年跟民國年放在一起有沒有搞錯啊
OTC_dispose = OTC_dispose %>% mutate(年 = substr(公布日期,1,3) %>% as.numeric + 1911) %>%
  mutate(年月日 = paste0(年,substr(公布日期,5,6),substr(公布日期,8,9))  %>% as.numeric) %>%
  select(-公布日期)

OTC_dispose = OTC_dispose %>% rename(c("證券代碼"="證券代號","公司名稱"="證券名稱", "處置條件"="處置原因")) %>%
                              mutate(證券代碼 = 證券代碼 %>% as.numeric) %>% na.omit %>% 
                              filter(substr(證券代碼,1,2) == "91" | 證券代碼 %between% c(1000,9999)) %>% 
                              arrange(證券代碼, desc(年月日) )




OTC_dispose = separate(OTC_dispose, 處置起訖時間 , c("處置開始期間","處置結束期間"),"~")

OTC_dispose = OTC_dispose %>% mutate(處置開始期間 = paste0((substr(處置開始期間,1,3) %>% as.numeric + 1911) 
                                                     ,substr(處置開始期間,5,6),substr(處置開始期間,8,9))  %>% as.numeric ,
                                     處置結束期間 = paste0((substr(處置結束期間,1,3) %>% as.numeric + 1911) 
                                                     ,substr(處置結束期間,5,6),substr(處置結束期間,8,9)) %>% as.numeric ) %>%
                              mutate( 處置實施天數 = ymd(處置結束期間) - ymd(處置開始期間) )

OTC_dispose = stoptime_func(OTC_dispose)

OTC_dispose$市場別 = "OTC" 
OTC_dispose = OTC_dispose[,c("證券代碼","公司名稱","市場別","年","年月日","處置開始期間","處置結束期間","處置次數","分盤","處置內容")]

table(OTC_dispose$分盤)

write.csv(OTC_dispose , "./tidy_up_data/處置股結果/上櫃處置股_20110101_20221018.csv" , row.names = F   )



#####載入股價資料表、與TSE、OTC資料合併####

###注意股資料合併###
TSE_notice = fread("./tidy_up_data/注意股結果/上市注意股_20110101_20221018.csv")
OTC_notice = fread("./tidy_up_data/注意股結果/上櫃注意股_20110101_20221018.csv")
all_notice = rbind(TSE_notice %>% select(-累計次數) , OTC_notice )

###處置股資料合併###
TSE_dispose = fread("./tidy_up_data/處置股結果/上市處置股_20110101_20221018.csv")
OTC_dispose = fread("./tidy_up_data/處置股結果/上櫃處置股_20110101_20221018.csv")
all_dispose = rbind( TSE_dispose , OTC_dispose ) %>% arrange(市場別,證券代碼,desc(年月日))

###載入股價資料表###
stock_price = fread("./orign_data/stock_price_2010101_20221201.txt" , header = T , sep = "\t" ,
                    colClasses = list(character = c(1,35:38) , numeric=2:34) )
gc()
stock_price = separate(stock_price, 證券代碼 , c("證券代碼","公司名稱")," " , extra = "merge" ) %>%  #水啦要加extra = "merge"
              mutate(證券代碼 = 證券代碼 %>% as.numeric)
gc()
#colnames(stock_price)
stock_price = stock_price %>% rename(c("TSE產業別" = "TSE 產業別" , "調整開盤價" = "開盤價(元)"   ,
                                       "最高價"="最高價(元)" , "最低價"="最低價(元)" ,
                                       "調整收盤價" = "收盤價(元)" , "成交張數" = "成交量(千股)" ,
                                       "成交金額_千元" = "成交值(千元)" , "報酬率" =  "報酬率％" ,
                                      "週轉率" = "週轉率％" )
                                      )

#合併注意股與股價資料
stock_price = merge(stock_price , all_notice %>% select(-公司名稱) , by = c("證券代碼","年月日","市場別" ) , all = T)

#合併處置股與股價資料
stock_price = merge(stock_price , all_dispose %>% select(-公司名稱,-年)  , by = c("證券代碼","年月日","市場別") , all = T)

##### 條款分組 #####
#新增分類指標，分成四大項目，漲跌，成家+漲跌，成交，其他
stock_price$條款分類 = NA
for (i in c(1:14) ) {
  if(i %in% c(1,2,5,7,11) ){ #漲跌
    stock_price = stock_price %>%　mutate( 條款分類 = ifelse(grepl( dic[i] ,  conditions ) , paste0(條款分類,"漲跌組",sep = "," ) , 條款分類 ))}  
  if(i %in% c(3,4)){ #成交+漲跌
    stock_price = stock_price %>%　mutate( 條款分類 = ifelse(grepl( dic[i] ,  conditions ) , paste0(條款分類,"成交量漲跌組",sep = "," ) , 條款分類 ))}
  if(i %in% c(9,10)){ #成交
    stock_price = stock_price %>%　mutate( 條款分類 = ifelse(grepl( dic[i] ,  conditions ) , paste0(條款分類,"成交量組",sep = "," ) , 條款分類 ))}
  if(i %in% c(6,8,12,13,14)){ #其他
    stock_price = stock_price %>%　mutate( 條款分類 = ifelse(grepl( dic[i] ,  conditions ) , paste0(條款分類,"其他",sep = "," ) , 條款分類 ))}
}
stock_price$條款分類 = gsub("NA","", stock_price$條款分類) #土法煉鋼把第一排NA刪除

#分類開始C4取1234 很白癡的做法，應該可以用choose 
stock_price = stock_price %>%　mutate(條款分類 = ifelse( grepl("漲跌組",條款分類) & !grepl("成交量漲跌組",條款分類) & #A
                                                      !grepl("成交量組",條款分類) & !grepl("其他",條款分類) , "漲跌組" , 條款分類 ),
                                     
                                      條款分類 = ifelse( grepl("漲跌組",條款分類) & grepl("成交量漲跌組",條款分類) & #AB
                                                      !grepl("成交量組",條款分類) & !grepl("其他",條款分類) , "漲跌組_成交量漲跌組" , 條款分類 ),
                                     
                                     條款分類 = ifelse( grepl("漲跌組",條款分類) & grepl("成交量漲跌組",條款分類) & #ABC
                                                       grepl("成交量組",條款分類) & !grepl("其他",條款分類) , "漲跌組_成交量漲跌組_成交量組" , 條款分類 ),
                                     
                                     條款分類 = ifelse( grepl("漲跌組",條款分類) & grepl("成交量漲跌組",條款分類) & #ABCD
                                                       grepl("成交量組",條款分類) & grepl("其他",條款分類) , "漲跌組_成交量漲跌組_成交量組_其他" , 條款分類 ),
                                      
                                     條款分類 = ifelse( !grepl("漲跌組",條款分類) & grepl("成交量漲跌組",條款分類) & #B
                                                       !grepl("成交量組",條款分類) & !grepl("其他",條款分類) , "成交量漲跌組" , 條款分類 ),
                                     
                                     條款分類 = ifelse( !grepl("漲跌組",條款分類) & !grepl("成交量漲跌組",條款分類) & #C
                                                       grepl("成交量組",條款分類) & !grepl("其他",條款分類) , "成交量組" , 條款分類 ),
                                      
                                     條款分類 = ifelse( !grepl("漲跌組",條款分類) & !grepl("成交量漲跌組",條款分類) & #D
                                                      !grepl("成交量組",條款分類) & grepl("其他",條款分類) , "其他" , 條款分類 ),
                                    
                                     條款分類 = ifelse( grepl("漲跌組",條款分類) & !grepl("成交量漲跌組",條款分類) & #AC
                                                      grepl("成交量組",條款分類) & !grepl("其他",條款分類) , "漲跌組_成交量組" , 條款分類 ),
                                     
                                     條款分類 = ifelse( grepl("漲跌組",條款分類) & !grepl("成交量漲跌組",條款分類) & #AD
                                                      !grepl("成交量組",條款分類) & grepl("其他",條款分類) , "漲跌組_其他" , 條款分類 ),
                                     
                                     條款分類 = ifelse( !grepl("漲跌組",條款分類) & grepl("成交量漲跌組",條款分類) & #BC
                                                       grepl("成交量組",條款分類) & !grepl("其他",條款分類) , "成交量漲跌組_成交量組" , 條款分類 ),
                                     
                                     條款分類 = ifelse( grepl("漲跌組",條款分類) & !grepl("成交量漲跌組",條款分類) & #BD
                                                      !grepl("成交量組",條款分類) & grepl("其他",條款分類) , "成交量漲跌組_其他" , 條款分類 ),
                                     
                                     條款分類 = ifelse( !grepl("漲跌組",條款分類) & !grepl("成交量漲跌組",條款分類) & #CD
                                                        grepl("成交量組",條款分類) & grepl("其他",條款分類) , "成交量組_其他" , 條款分類 ),
                                     
                                     條款分類 = ifelse( grepl("漲跌組",條款分類) & grepl("成交量漲跌組",條款分類) & #ABD
                                                       !grepl("成交量組",條款分類) & grepl("其他",條款分類) , "漲跌組_成交量漲跌組_其他" , 條款分類 ),
                                     
                                     條款分類 = ifelse( grepl("漲跌組",條款分類) & !grepl("成交量漲跌組",條款分類) & #ACD
                                                       grepl("成交量組",條款分類) & grepl("其他",條款分類) , "漲跌組_成交量組_其他" , 條款分類 ),
                                     
                                     條款分類 = ifelse( !grepl("漲跌組",條款分類) & grepl("成交量漲跌組",條款分類) & # BCD
                                                       grepl("成交量組",條款分類) & grepl("其他",條款分類) , "成交量漲跌組_成交量組_其他" , 條款分類 )
                                     
                                                                          )
#上面是白癡分組寫法
stock_price = stock_price %>% mutate( 條款分類 = ifelse( grepl("成交量組_其他",條款分類), "成交量組" , 條款分類 ),
                                      條款分類 = ifelse( grepl("漲跌組_成交量組",條款分類), "成交量漲跌組" , 條款分類 ),
                                      條款分類 = ifelse( grepl("漲跌組_成交量組_其他",條款分類), "成交量漲跌組" , 條款分類 ),
                                      條款分類 = ifelse( grepl("漲跌組_成交量漲跌組",條款分類), "成交量漲跌組" , 條款分類 ),
                                      條款分類 = ifelse( grepl("漲跌組_成交量漲跌組_其他",條款分類), "成交量漲跌組" , 條款分類 )
                                      ) %>% 
                              mutate( 條款分類 = factor( 條款分類 , order = T , levels = c("漲跌組","成交量組","成交量漲跌組","其他") ) ) #在這邊加入factor排序
             


table(stock_price$條款分類)
tmp = stock_price %>% filter(is.na(conditions) ==T , is.na(條款分類) == F)
tmp = stock_price %>% filter(is.na(conditions) ==F , is.na(條款分類) == T)

gc()


#等下要做的事就是先把stock_price的欄位先整理一下並且算出進一日報酬跟進六日報酬，以便判斷他是第一款漲還是第一款跌

##### 資料檢查、資料庫比對#####
#cc = stock_price[,c("收盤價.x","收盤價.y")] %>% na.omit
#檢查有沒有有公告但是卻沒有股價資料的，nono
#cc = stock_price[is.na(證券代碼)== T ,] %>% arrange(conditions)  

#問題 : TEJ股價表裡面有的注意股票，但證交所下載下來的卻沒有 有可能是因為，還沒合併OTC的資料，
#確定問題 : DR股票會被排除，難怪第八款趨近於0，再刪股票的時候需要排除91開頭的股票

#檢查上市注意股資料
#檢查有沒有證交所沒有資料，但TEJ確有資料
#cc = stock_price_test[,c(1:6,36:39,43:45)] %>%  filter(市場別 == "TSE"  ,stock_price_test$`注意股票(A)` == "A" , is.na(conditions) == T )
#檢查有沒有證交所有資料，TEJ沒有資料
#cc = stock_price_test[,c(1:3,36:39,43:45)] %>%  filter( stock_price_test$`注意股票(A)` == "." , is.na(conditions) == F ) #有三筆

#證交所下載下來的資料TEJ全部都有，但TEJ有的證交所沒有
#cc_8070 = stock_price_test[年月日 %between% c(20200801,20200831) & 證券代碼 == 8070, ] 
#cc_910801 = stock_price_test[年月日 %between% c(20140601,20140701) & 證券代碼 == 910801, ] 
# 9103 美德醫療確定證交所20140310 沒有注意股訊息 (證交所14年3月有07,12兩筆，但就沒有0310)
# 8070 長華*證交所有20200818，但就沒有20200817 #這不好處理，因為確實是0817當天成交資訊異常，但是證交所資料是18號，TEJ反而比較準!?
# 910801 證交所指查的到6月19號，查不到0604
#怎麼辦勒

#檢查上櫃注意股資料
#檢查有沒有櫃買中心沒有資料，但TEJ確有資料
cc = stock_price_test[,c(1:7,37:39,43:44)] %>%  filter(市場別 == "OTC"  ,stock_price_test$`注意股票(A)` == "A" , is.na(conditions) == T ) #86筆
# 櫃買中心很屌啦列交易資訊的公司但沒列第幾款，我現在是要自己猜就是了 有85筆
# 先不管他

#檢查有沒有櫃買中心有資料，但TEJ沒有資料
cc = stock_price_test[,c(1:7,37:39,43:44)] %>%  filter( stock_price_test$`注意股票(A)` == "." , is.na(conditions) == F ) #32筆
# 這個問題比較小，以櫃買中心為準即可
# 
# 證交所3筆，櫃買中心85筆資料資之缺失值不理他。總比數共41998筆，忽略其中88筆移除缺失值。



##### 漲跌分組與報酬率計算 #####
#####我需要求出每個條款的平均報酬率，t1~t20，先不看漲跌的
stock_price = stock_price %>% group_by(證券代碼) %>%
  mutate( 漲跌分組 = ifelse((調整收盤價 -lag(調整收盤價 ,5)) / lag(調整收盤價 , 5) >= 0 , "上漲" , "下跌" )) %>% ungroup
#漲跌分組，近六日漲跌，用做判斷條款漲跌用途

##### for 第二款使用 30日漲跌 60日漲跌 90日漲跌
stock_price = stock_price %>% group_by(證券代碼) %>%
  mutate( 當天收盤減開盤 = ifelse((調整收盤價 - 調整開盤價) > 0 ,"上漲" , "下跌" ),
          當天跟前30天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,29)) / lag(調整收盤價,29)) > 0 , "上漲" , "下跌"),
          當天跟前60天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,59)) / lag(調整收盤價,59)) > 0 , "上漲" , "下跌"),
          當天跟前90天報酬 = ifelse(((調整收盤價 - lag(調整收盤價,89)) / lag(調整收盤價,89)) > 0 , "上漲" , "下跌")
          #三十日最高 = rollmax(x = 調整收盤價 , k = 10 , align = "right" , fill = NA),
          #三十日最低 = rollmax(x = -調整收盤價 , k = 10 , align = "right" , fill = NA) %>% abs ,
  )

#第二款漲跌判斷 ，要把上面的縮減到一個欄位
stock_price = stock_price %>% group_by(證券代碼) %>%
  mutate(  第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("三十", 注意交易資訊) & 當天跟前30天報酬 == "上漲","上漲","下跌"),
           第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("六十", 注意交易資訊) & 當天跟前60天報酬 == "上漲", "上漲" , 第二款漲跌 ) ,
           第二款漲跌 = ifelse( grepl("第二款", conditions) & grepl("九十", 注意交易資訊) & 當天跟前90天報酬 == "上漲", "上漲" , 第二款漲跌 )
  )

#stock_price %>% filter(is.na(調整收盤價) == T )

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

##### 算出提前持有1天報酬走勢 #####
for (ndays in 1:10){
  if (ndays == 0){ stock_price = stock_price %>% group_by(證券代碼) %>% 
    mutate( 持有0日報酬 = (調整收盤價-調整開盤價)/調整開盤價) } %>% ungroup #持有0日是偷看
  if(ndays != 0){
    stock_price = stock_price %>% group_by(證券代碼) %>% 
      mutate( holding_ndays = ( lead(調整收盤價, n = ndays) - lead(調整開盤價, n = 1)  )/lead(調整開盤價, n = 1)) %>% ungroup
    colnames(stock_price)[NCOL(stock_price)]  = paste0("持有",ndays,"日報酬")
  }
}

#儲存股價資料表合併注意股
write.csv(stock_price , "./tidy_up_data/合併後股價資料表_20100101_20221231.csv" , row.names = F)

# 第六款分類有點詭異，可以看一下是不是可以細分















