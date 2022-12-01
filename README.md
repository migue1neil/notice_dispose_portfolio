# 警示股下的投資策略
 
# 簡介 
施工中，目前專案是希望可以根據最新的法規去看在甚麼類型的條件下股價表現最好。
2022/11/30更新
目前已知問題為，雖然都是在同一個注意股條款中，但每款條款的平均報酬差異都非常大，標準差也非常大，正在思考怎麼解決這個問題。

# 程式功能
目前功能 :
注意股的部份，由於法規不同，上市上櫃股票列入注意股的條款不一樣，本程式根據現有法規(111年1月)將兩個市場條款整理為一至，方便後續做量化回測與事件日交易。同時，也將注意股條款分成漲跌、漲跌+成交量、成交量、其他，四個項目來進行分析。
處置股的部份，目前分類有第一次處置、第二次處置，與分盤時間，將這些分組跑出事件發生時的平均報酬。

未來功能 :
預判即將進入警示股的股票，研擬交易策略。
