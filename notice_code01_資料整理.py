# -*- coding: utf-8 -*-
"""
Created on Wed Jan 11 15:00:59 2023

@author: Neil
"""
import os
import pandas as pd
import numpy as np
#from datetime import datetime
import re

os.chdir("C:/Users/Neil/Documents/git-repos/")

# 警示股交易復刻
dic = ["第一款","第二款","第三款","第四款","第五款","第六款","第七款","第八款",
        "第九款","第十款","第十一款","第十二款","第十三款","第十四款" ]
#注意股資料
###############################################################################
#載入上市櫃注意股
TSE_notice = pd.read_csv("./notice_stock_portfolio/orign_data/上市注意股2011_2022.csv", encoding= 'mbcs' , sep = ",").iloc[:,1:9]
TSE_notice = TSE_notice.rename( columns= {'證券代號':'證券代碼' , '證券名稱':'公司名稱' , "日期":"年月日"}  )
TSE_notice = TSE_notice[pd.notnull(TSE_notice['證券代碼'])]
# 民國年轉換成西元年(嘗試不用迴圈) 
def tran_date_func(date_series , sep):       
    sep = re.escape(sep) #避免被當成正規表示法
    tmp = pd.DataFrame()    
    tmp[['年','月',"日"]] = date_series.str.split( sep , expand = True , n = 2)
    #先都轉換成數字
    tmp["年"] = tmp["年"].astype(int)
    tmp["月"] = tmp["月"].astype(int)
    tmp["日"] = tmp["日"].astype(int)   
    tmp["年"] = np.where(tmp["年"] > 1911 ,tmp["年"] , tmp["年"] + 1911  ) #這個好用類似ifelse()       
    #這邊開始把月跟日補零    
    tmp["月"] = tmp["月"].apply(lambda x: '0' + str(x) if x < 10 else str(x))
    tmp["日"] = tmp["日"].apply(lambda x: '0' + str(x) if x < 10 else str(x))
    #先轉換成字串相加之後再轉換成數字
    tmp["年月日"] = tmp["年"].astype(str) +tmp["月"].astype(str)+tmp["日"].astype(str)
    date_series = tmp["年月日"].astype(int)
    return(date_series)
    
TSE_notice["年月日"] = tran_date_func(TSE_notice["年月日"] , sep = "." )

# 排除非上市櫃公司
# 刪除有代號的股票
def del_nonstock(TSE_notice):
    TSE_notice['證券代碼'] = np.where( TSE_notice['證券代碼'].str.isdigit() , TSE_notice['證券代碼'] , None ) 
    TSE_notice = TSE_notice[pd.notnull(TSE_notice['證券代碼'])]
    TSE_notice['證券代碼'] = TSE_notice['證券代碼'].astype(int)
    TSE_notice = TSE_notice.query('1000 < 證券代碼 < 9999' )
    return TSE_notice

TSE_notice = del_nonstock(TSE_notice)

#新增市場別
TSE_notice['市場別'] = "TSE"

#處理本益比資料
TSE_notice['本益比'] = TSE_notice['本益比'].str.strip().apply(lambda x : re.sub("-----",'',x) ) #本益比還是字串，要想一下怎麼用

#處理條款
def add_conditions_column(df):
    df["conditions"] = None
    for i in range(1, 14):
        df["conditions"] = np.where(df["注意交易資訊"].str.contains(dic[i - 1]), 
                                     df["conditions"].astype(str) + "," + dic[i - 1], 
                                     df["conditions"])

    df["conditions"] = df["conditions"].str.replace("None,", "")
    df["conditions"] = np.where(df["注意交易資訊"].str.contains("監視業務督導會報"), 
                                 "第十四款", df["conditions"])
    return df

TSE_notice = add_conditions_column(TSE_notice)

def modify_conditions(df, date, condition_column):
    mask = df['年月日'] < date
    
    df.loc[mask & df[condition_column].str.contains("第六款"), condition_column] = df.loc[mask & df[condition_column].str.contains("第六款"), condition_column].str.replace("第六款", "9")
    df.loc[mask & df[condition_column].str.contains("第七款"), condition_column] = df.loc[mask & df[condition_column].str.contains("第七款"), condition_column].str.replace("第七款", "10")
    df.loc[mask & df[condition_column].str.contains("第八款"), condition_column] = df.loc[mask & df[condition_column].str.contains("第八款"), condition_column].str.replace("第八款", "6")
    df.loc[mask & df[condition_column].str.contains("第九款"), condition_column] = df.loc[mask & df[condition_column].str.contains("第九款"), condition_column].str.replace("第九款", "7")
    df.loc[mask & df[condition_column].str.contains("第十款"), condition_column] = df.loc[mask & df[condition_column].str.contains("第十款"), condition_column].str.replace("第十款", "8")
    
    df[condition_column] = df[condition_column].str.replace("6", "第六款")
    df[condition_column] = df[condition_column].str.replace("7", "第七款")
    df[condition_column] = df[condition_column].str.replace("8", "第八款")
    df[condition_column] = df[condition_column].str.replace("9", "第九款")
    df[condition_column] = df[condition_column].str.replace("10", "第十款")
    
    return df

TSE_notice = modify_conditions(TSE_notice, date = 20150811 , condition_column = 'conditions')


#設計一個敘述統計表
def generate_stat_sheet(TSE_notice , mode = "default"):
    
    dic = ["第一款","第二款","第三款","第四款","第五款","第六款","第七款","第八款",
            "第九款","第十款","第十一款","第十二款","第十三款","第十四款" ]
    dic_group = ["漲跌組","成交量組","價量組","其他"]
    dic_dispose = ["第一次處置","第二次處置",'人工管制撮合','監視業務督導會報']
    
    if mode == "default" :
        switch_var = dic
        coln = "conditions"
    elif mode == "group" :
        switch_var = dic_group
        coln = "條款分類"
    elif mode == "dispose" :
        switch_var = dic_dispose
        coln = "處置措施"      
    else :
        print( "something wrong!" )
        return 
      
    stat_sheet = pd.DataFrame()
    for year in range(2011, 2023):
        sheet = pd.DataFrame()
        TSE_notice['year'] = pd.to_datetime(TSE_notice['年月日'], format='%Y%m%d').apply(lambda x: x.year)
        for i in range(len(switch_var)):
            condition_name = switch_var[i]
            n_count = TSE_notice[(TSE_notice['year'] == year) & 
                                 TSE_notice[f'{coln}'].str.contains(switch_var[i])].shape[0]
            tmp_sheet = pd.DataFrame({condition_name: [n_count]})
            sheet = pd.concat([sheet, tmp_sheet], axis=1)
        tmp_sheet = pd.DataFrame({'年': [year], **sheet})
        stat_sheet = pd.concat([stat_sheet, tmp_sheet], axis=0) 
        
    # 新增合計欄位
    table_year = stat_sheet[["年"]].astype(str)
    table_year.loc[len(table_year)] = "合計"
    table_content = stat_sheet.iloc[:,1:len(stat_sheet.columns)] #找出後面的內容
    sum_of_columns = table_content.sum() #在最後一排新增數字的用法
    table_content.loc[len(table_content)] = sum_of_columns   
    stat_sheet = pd.concat([table_year , table_content] , axis = 1).reset_index(drop = True)  
    return stat_sheet

TSE_stat_sheet = generate_stat_sheet(TSE_notice )

# 保存統計表格
TSE_stat_sheet.to_csv("./notice_stock_portfolio/tidy_up_data/注意股結果/上市注意股出現次數表格.csv", index=False)
# 保存上市注意股數據
# TSE_notice.to_csv("./notice_stock_portfolio/tidy_up_data/注意股結果/上市注意股_20110101_20221231.csv", index=False)

#上櫃注意股
OTC_notice = pd.read_csv("./notice_stock_portfolio/orign_data/上櫃注意股2011_2022.csv", encoding= 'mbcs' , sep = ",").iloc[:,1:9]

#基本整理
OTC_notice = OTC_notice.rename( columns= {'證券代號':'證券代碼' , '證券名稱':'公司名稱' , "公告日期":"年月日"}  )
OTC_notice = OTC_notice[ pd.notnull(OTC_notice['證券代碼']) ]
OTC_notice['市場別'] = "OTC"

#排除非普通股
OTC_notice = del_nonstock(OTC_notice)

#轉換西元年
OTC_notice["年月日"] = tran_date_func(OTC_notice["年月日"] , sep = "/" )
OTC_notice = OTC_notice[OTC_notice['年月日']>20110101 ]  #移除小於2011年的資料

#新增條款
OTC_notice = add_conditions_column(OTC_notice)

#移除沒有被標記條款的股票 全為2011年
OTC_notice= OTC_notice [ pd.notnull(OTC_notice["conditions"] )]

#更新條款 日期 
OTC_notice = modify_conditions(OTC_notice, date = 20190410 , condition_column = 'conditions')

#上櫃敘述統計表
OTC_stat_sheet = generate_stat_sheet(OTC_notice)

# 保存統計表格
OTC_stat_sheet.to_csv("./notice_stock_portfolio/tidy_up_data/注意股結果/上櫃注意股出現次數表格.csv", index=False)
# 保存上市注意股數據
# OTC_notice.to_csv("./notice_stock_portfolio/tidy_up_data/注意股結果/上櫃注意股_20110101_20221231.csv", index=False)

# 合併注意股資料
# 移除不需要的上市注意股欄位並從新排序
TSE_notice = TSE_notice[['證券代碼','公司名稱','市場別','年月日','conditions','注意交易資訊']]
OTC_notice = OTC_notice[['證券代碼','公司名稱','市場別','年月日','conditions','注意交易資訊']]
all_notice = pd.concat([TSE_notice,OTC_notice ] , axis = 0  )

#全部股票出現次數表格
all_stat_sheet = generate_stat_sheet(all_notice)
all_stat_sheet.to_csv("./notice_stock_portfolio/tidy_up_data/注意股結果/全部注意股出現次數表格.csv", index=False)

# 把注意股根據條款性質進行分類
all_notice["條款分類"] = None
for i in range(15) :  
    if i in [1,2,5,7,11]: #漲跌
        print(dic[i-1])
        all_notice['條款分類'] = np.where( all_notice['conditions'].str.contains(dic[i-1]) , all_notice['條款分類'].astype(str) + "漲跌組" , all_notice['條款分類'] ) 
    if i in [3,4]:  #成交+漲跌
        print(dic[i-1])
        all_notice['條款分類'] = np.where( all_notice['conditions'].str.contains(dic[i-1]) , all_notice['條款分類'].astype(str) + "價量組" , all_notice['條款分類'] ) 
    if i in [9,10]: #成交量組
        print(dic[i-1])
        all_notice['條款分類'] = np.where( all_notice['conditions'].str.contains(dic[i-1]) , all_notice['條款分類'].astype(str) + "成交量組" , all_notice['條款分類'] ) 
    if i in [6,8,12,13,14]: #其他
        print(dic[i-1])
        all_notice['條款分類'] = np.where( all_notice['conditions'].str.contains(dic[i-1]) , all_notice['條款分類'].astype(str) + "其他" , all_notice['條款分類'] ) 
        
all_notice["條款分類"] = all_notice["條款分類"].str.replace("None","")

all_notice['條款分類'] = np.where( (all_notice['條款分類'].str.contains('漲跌組') & ~all_notice['條款分類'].str.contains('價量組') & #A
                                      ~all_notice['條款分類'].str.contains('成交量組') & ~all_notice['條款分類'].str.contains('其他')), "漲跌組", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (all_notice['條款分類'].str.contains('漲跌組') & all_notice['條款分類'].str.contains('價量組') &  #AB
                                      ~all_notice['條款分類'].str.contains('成交量組') & ~all_notice['條款分類'].str.contains('其他')), "價量組", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (all_notice['條款分類'].str.contains('漲跌組') & ~all_notice['條款分類'].str.contains('價量組') &  #AC
                                     all_notice['條款分類'].str.contains('成交量組') & ~all_notice['條款分類'].str.contains('其他')), "價量組", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (all_notice['條款分類'].str.contains('漲跌組') & ~all_notice['條款分類'].str.contains('價量組') &  #AD
                                     ~all_notice['條款分類'].str.contains('成交量組') & all_notice['條款分類'].str.contains('其他')), "其他", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (all_notice['條款分類'].str.contains('漲跌組') & all_notice['條款分類'].str.contains('價量組') & #ABC
                                      all_notice['條款分類'].str.contains('成交量組') & ~all_notice['條款分類'].str.contains('其他')), "價量組", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (all_notice['條款分類'].str.contains('漲跌組') & all_notice['條款分類'].str.contains('價量組') & #ABD
                                      ~all_notice['條款分類'].str.contains('成交量組') & all_notice['條款分類'].str.contains('其他')), "其他", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (all_notice['條款分類'].str.contains('漲跌組') & ~all_notice['條款分類'].str.contains('價量組') & #ACD
                                     all_notice['條款分類'].str.contains('成交量組') & all_notice['條款分類'].str.contains('其他')), "其他", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (all_notice['條款分類'].str.contains('漲跌組') & all_notice['條款分類'].str.contains('價量組') & #ABCD
                                      all_notice['條款分類'].str.contains('成交量組') & all_notice['條款分類'].str.contains('其他')), "其他", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (~all_notice['條款分類'].str.contains('漲跌組') & all_notice['條款分類'].str.contains('價量組') & #B
                                      ~all_notice['條款分類'].str.contains('成交量組') & ~all_notice['條款分類'].str.contains('其他')), "價量組", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (~all_notice['條款分類'].str.contains('漲跌組') & all_notice['條款分類'].str.contains('價量組') & #BC
                                      all_notice['條款分類'].str.contains('成交量組') & ~all_notice['條款分類'].str.contains('其他')), "價量組", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (~all_notice['條款分類'].str.contains('漲跌組') & all_notice['條款分類'].str.contains('價量組') & #BD
                                      ~all_notice['條款分類'].str.contains('成交量組') & all_notice['條款分類'].str.contains('其他')), "其他", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (~all_notice['條款分類'].str.contains('漲跌組') & all_notice['條款分類'].str.contains('價量組') & #BCD
                                      all_notice['條款分類'].str.contains('成交量組') & all_notice['條款分類'].str.contains('其他')), "其他", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (~all_notice['條款分類'].str.contains('漲跌組') & ~all_notice['條款分類'].str.contains('價量組') & #C
                                      all_notice['條款分類'].str.contains('成交量組') & ~all_notice['條款分類'].str.contains('其他')), "成交量組", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (~all_notice['條款分類'].str.contains('漲跌組') & ~all_notice['條款分類'].str.contains('價量組') & #CD
                                      all_notice['條款分類'].str.contains('成交量組') & all_notice['條款分類'].str.contains('其他')), "其他", all_notice['條款分類'])

all_notice['條款分類'] = np.where( (~all_notice['條款分類'].str.contains('漲跌組') & ~all_notice['條款分類'].str.contains('價量組') & #D
                                      ~all_notice['條款分類'].str.contains('成交量組') & all_notice['條款分類'].str.contains('其他')), "其他", all_notice['條款分類'])

# 統計注意股出現次數
all_notice_stat = generate_stat_sheet(all_notice , mode ="group" )
all_notice_stat.to_csv("./notice_stock_portfolio/tidy_up_data/注意股結果/條款性質分組_全部注意股出現次數表格.csv", index=False)

#儲存統計表格
all_notice = all_notice [ all_notice["年月日"] <= 20221231 ].reset_index(drop = True)
all_notice = all_notice[['證券代碼','公司名稱','市場別','年月日','conditions','條款分類','注意交易資訊']]
all_notice.to_csv("./notice_stock_portfolio/tidy_up_data/注意股結果/all_notice_2011_2022.csv", index=False)

#到這邊注意股的部分就處理完了。
#移除用不到的變數
del i,OTC_notice,TSE_notice
#all_notice_stat.iloc[-1,1:5].sum() #會有差異是因為沒排除2023年
###############################################################################

#上市處置股資料
TSE_dispose = pd.read_csv("./notice_stock_portfolio/orign_data/上市處置股2011_2022.csv", encoding= 'mbcs' , sep = ",").iloc[:,1:9]
TSE_dispose['市場別'] = "TSE"

#先調整日期
TSE_dispose = TSE_dispose.rename( columns= {'證券代號':'證券代碼' , '證券名稱':'公司名稱' , "公布日期":"年月日"}  )
TSE_dispose = TSE_dispose[pd.notnull(TSE_dispose["年月日"])]
TSE_dispose["年月日"] = tran_date_func(TSE_dispose["年月日"] , sep = "/" )
TSE_dispose = TSE_dispose[(TSE_dispose["年月日"] >= 20110101) & (TSE_dispose["年月日"] <= 20221231)]

#移除非普通股
TSE_dispose = del_nonstock(TSE_dispose)

#上櫃處置股資料
OTC_dispose = pd.read_csv("./notice_stock_portfolio/orign_data/上櫃處置股2011_2022.csv", encoding= 'mbcs' , sep = ",").iloc[:,1:9]
OTC_dispose['市場別'] = "OTC"

#調整日期時間
OTC_dispose = OTC_dispose.rename( columns= {'證券代號':'證券代碼' , '證券名稱':'公司名稱' , "公布日期":"年月日"}  )
OTC_dispose = OTC_dispose[pd.notnull(OTC_dispose["年月日"])]
OTC_dispose["年月日"] = tran_date_func(OTC_dispose["年月日"] , sep = "/" )
OTC_dispose = OTC_dispose[(OTC_dispose["年月日"] >= 20110101) & (OTC_dispose["年月日"] <= 20221231)]

#移除非普通股
OTC_dispose = del_nonstock(OTC_dispose)

#新增處置次數欄位 人工方式判斷是第幾次處置
OTC_dispose['處置次數'] = None
OTC_dispose['處置次數'] = np.where( OTC_dispose['處置內容'].str.contains("曾發布") , "第二次處置" ,  OTC_dispose['處置次數']) 
OTC_dispose['處置次數'] = np.where( (OTC_dispose['處置原因'].str.contains("最近10個營業日內有6個營業日")) & ( pd.isna(OTC_dispose['處置次數']) ) ,                                                            
                               "第一次處置" ,  OTC_dispose['處置次數'])
OTC_dispose['處置次數'] = np.where( (OTC_dispose['處置原因'].str.contains("連續5個營業日")) & ( pd.isna(OTC_dispose['處置次數']) ) ,                                                            
                               "第一次處置" ,  OTC_dispose['處置次數'])
OTC_dispose['處置次數'] = np.where( (OTC_dispose['處置原因'].str.contains("因連續3個營業日達本中心作業要點第四條第一項第一款")) & ( pd.isna(OTC_dispose['處置次數']) ) ,                                                            
                               "第一次處置" ,  OTC_dispose['處置次數'])
OTC_dispose['處置次數'] = np.where( (OTC_dispose['處置原因'].str.contains("最近30個營業日內有12個營業日")) & ( pd.isna(OTC_dispose['處置次數']) ) ,                                                            
                               "第一次處置" ,  OTC_dispose['處置次數'])
OTC_dispose['處置次數'] = np.where( (OTC_dispose['處置原因'].str.contains("監視業務督導會報")) & ( pd.isna(OTC_dispose['處置次數']) ) ,                                                            
                               "監視業務督導會報" ,  OTC_dispose['處置次數'])

OTC_dispose= OTC_dispose[ ~OTC_dispose['處置內容'].str.contains("恢復為")] #移除恢復處置的股票，只有一筆 泰谷

#合併兩表
TSE_dispose = TSE_dispose[['證券代碼','公司名稱','市場別','年月日','處置起迄時間','處置措施','處置內容']]
OTC_dispose = OTC_dispose.rename( columns= {'處置起訖時間':'處置起迄時間' , '處置次數':'處置措施' } )
OTC_dispose = OTC_dispose[['證券代碼','公司名稱','市場別','年月日','處置起迄時間','處置措施','處置內容']]
all_dispose = pd.concat([TSE_dispose ,OTC_dispose ] , axis = 0 ).sort_values("年月日" , ascending = False).reset_index(drop = True)

#敘述統計表
TSE_dispose_stat = generate_stat_sheet(TSE_dispose , mode = "dispose")
OTC_dispose_stat = generate_stat_sheet(OTC_dispose , mode = "dispose")
all_dispose_stat = generate_stat_sheet(all_dispose , mode = "dispose")

#存檔
TSE_dispose_stat.to_csv("./notice_stock_portfolio/tidy_up_data/處置股結果/上市處置股出現次數表格.csv", index=False)
OTC_dispose_stat.to_csv("./notice_stock_portfolio/tidy_up_data/處置股結果/上櫃處置股出現次數表格.csv", index=False)
all_dispose_stat.to_csv("./notice_stock_portfolio/tidy_up_data/處置股結果/全部處置股出現次數表格.csv", index=False)
all_dispose.to_csv("./notice_stock_portfolio/tidy_up_data/處置股結果/all_dispose2011_2022.csv", index=False)

# 分類要新增一個監督業務督導會報決議 另外拿出來看
# 監視業務督導會報
# 最近30個營業日內有12個營業日

#處置資料處裡結束
###############################################################################
#載入股價資料表

stock_price = pd.read_table("./notice_stock_portfolio/orign_data/stock_price20100101_20230212.txt", encoding= 'mbcs' , sep = ",")
stock_price[['證券代碼','公司名稱']] = stock_price['證券代碼'].str.split(" ", expand = True,n = 1)
stock_price = stock_price.rename( columns= {'TSE 產業別':'產業別'}  )
stock_price.columns

#移除非普通股
stock_price = del_nonstock(stock_price)
stock_price = pd.merge( stock_price , all_notice.drop(columns=['公司名稱','市場別']) , on = ['證券代碼','年月日'] , how = "outer" )
stock_price = pd.merge( stock_price , all_dispose.drop(columns=['公司名稱','市場別']) , on = ['證券代碼','年月日'] , how = "outer" )

#瘦身
#沒有被列為注意股/處置股的就移除
keep_stock = list(set(all_notice['證券代碼'].unique().tolist() + all_dispose['證券代碼'].unique().tolist()))

stock_price = stock_price[stock_price['證券代碼'].isin(keep_stock)]

#移除掛牌日後五天的資料
stock_price = stock_price[pd.notnull(stock_price['首次TSE上市日'])]
stock_price = stock_price[pd.notnull(stock_price['首次OTC上市日'])]
stock_price['首次TSE上市日'] = stock_price['首次TSE上市日'].str.replace(".","99999999" , regex = False).astype(int)
stock_price['首次OTC上市日'] = stock_price['首次OTC上市日'].str.replace(".","99999999" , regex = False).astype(int)
stock_price['最早掛牌日'] = np.where( stock_price['首次TSE上市日'] > stock_price['首次OTC上市日'] , stock_price['首次OTC上市日'] , stock_price['首次TSE上市日']  )
stock_price = stock_price[((stock_price['年月日'] - stock_price['最早掛牌日']) > 5)]


stock_price = stock_price[['證券代碼','公司名稱','市場別','產業別','年月日','開盤價(元)','收盤價(元)','成交量(千股)','報酬率％', '週轉率％',
             '市值(百萬元)','本益比-TSE','股價淨值比-TSE','漲跌停','注意股票(A)', '處置股票(D)', '全額交割(Y)','conditions', '條款分類',
             '注意交易資訊', '處置起迄時間', '處置措施', '處置內容' ]]

stock_price.columns
stock_price = stock_price.rename( columns= {'開盤價(元)':'調整開盤價' , "收盤價(元)":"調整收盤價" , "成交量(千股)":"成交張數",
                              '報酬率％' : "close_ret", 
                              }  )
stock_price['close_ret'] = stock_price['close_ret']/100

#漲跌分組計算 算出近六日是漲是跌
stock_price['漲跌分組'] = np.where( (stock_price['調整收盤價'] - stock_price.groupby('證券代碼')["調整收盤價"].shift(5)) > 0, "up" , "down") 
stock_price['漲跌停'] = stock_price['漲跌停'].replace("+","up")
stock_price['漲跌停'] = stock_price['漲跌停'].replace("-","down")

stock_price.to_csv("./notice_stock_portfolio/tidy_up_data/stock_price2011_2022.csv", index=False)


































