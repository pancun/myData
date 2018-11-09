## =============================================================================
## updating_info.R
## 每日更新 股票基础信息 数据
## 
## AUTHOR   : fl@hicloud-investment.com
## DATE     : 2018-04-15
## =============================================================================

suppressMessages({
    suppressMessages({
        source("/home/fl/myData/R/Rconfig/myInit.R")
    })
})

## ----------------------------------
## 18:00 更新数据
tradingDay <- lastTradingDay[1, days]
## ----------------------------------

## =====================================================
## 更新 股票指数 列表
url <- "http://summary.jrj.com.cn/zslbsh.shtml"
exchID <- 'sh'
sh <- fetch_index_lists_from_jrj(url, exchID)

url <- "http://summary.jrj.com.cn/zslbsz.shtml"
exchID <- 'sz'
sz <- fetch_index_lists_from_jrj(url, exchID)

dt <- list(sh, sz) %>% 
    rbindlist() %>% 
    .[, .(indexID, indexName, exchID)]
mysqlWrite(db = 'china_stocks_info', tbl = 'index_list',
           data = dt)
## =====================================================



