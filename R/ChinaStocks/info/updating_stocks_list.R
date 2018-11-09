## =============================================================================
## updating_stocks_list.R
## 每日更新 交易所 股票列表
## 用于盘前交易
## 
## AUTHOR   : fl@hicloud-investment.com
## DATE     : 2018-04-15
## =============================================================================

setwd('/home/fl/myData/')
suppressMessages({
  source('./R/Rconfig/myInit.R')
  source('./R/Rconfig/stocks.R')
})
library(httr)
library(rjson)

if (!format(Sys.Date(), '%Y-%m-%d') %in% ChinaStocksCalendar$days) {
    stop('Not TradignDay !!!')
}

## =============================================================================
## 从 交易所 下载
# fetch_stocks_list_from_exch()
source("./R/ChinaStocks/info/fromExch_stocks_list.R")
## =============================================================================
