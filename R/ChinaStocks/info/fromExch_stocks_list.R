## =============================================================================
## fromExch_stocksList.R
##
## 用于获取 上交所 深交所 股票列表
##
## Author : fl@hicloud-investment.com
## Date   : 2018-01-10
## =============================================================================

## =============================================================================
# setwd('/home/fl/myData/')
# suppressMessages({
#   source('./R/Rconfig/myInit.R')
#   source('./R/Rconfig/stocks.R')
# })
# library(httr)
# library(rjson)

# if (!format(Sys.Date(), '%Y-%m-%d') %in% ChinaStocksCalendar$days) {
#     stop('Not TradignDay !!!')
# }

tday <- format(Sys.Date(), '%Y%m%d')
## =============================================================================


## =============================================================================
tryNo <- 0
while (tryNo < 5) {
    dt_sse <- fetch_sse_listing()
    if (nrow(dt_sse) != 0) break
    tryNo <- tryNo + 1
}

if (nrow(dt_sse) != 0) {
    mysqlSend(db = 'china_stocks_info',
              query = "delete from stocks_list where exchID = 'sh'")
    mysqlWrite(db = 'china_stocks_info',
               tbl = 'stocks_list',
               data = dt_sse)
}

fwrite(dt_sse[order(listingDate)],
       sprintf("/data/ChinaStocks/Info/Listing/%s_sse.csv", tday)
      )
## =============================================================================


## =============================================================================
tryNo <- 0
while (tryNo < 5) {
    dt_szse <- fetch_szse_listing()
    if (nrow(dt_szse) != 0) break
    tryNo <- tryNo + 1
}

if (nrow(dt_szse) != 0) {
    mysqlSend(db = 'china_stocks_info',
              query = "delete from stocks_list where exchID = 'sz'")
    mysqlWrite(db = 'china_stocks_info',
               tbl = 'stocks_list',
               data = dt_szse)
}

fwrite(dt_szse[order(listingDate)],
       sprintf("/data/ChinaStocks/Info/Listing/%s_szse.csv", tday)
      )
## =============================================================================

