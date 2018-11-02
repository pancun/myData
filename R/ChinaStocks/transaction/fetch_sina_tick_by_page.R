## =============================================================================
## 从新浪财经下载股票历史成交明细
## ------------------------------
## @Author: “williamlfang”
## @Date:   2018-10-14 10:02:28
## @Last Modified by: “williamlfang”
## @Last Modified at: 2018-10-14 10:03:18
## =============================================================================

setwd('/home/fl/myData/')
suppressMessages({
    source('./R/Rconfig/myInit.R')
})
csvPath <- "/data/ChinaStocks/TickData/FromSinaCsv"

## -----------------------------------------------------------------------------
allStocks <- mysqlQuery(db = 'china_stocks_info',
                        query = 'select * from stocks_list
                                 order by stockID')

ChinaStocksCalendar <- ChinaStocksCalendar[days %between% c('2004-10-08', format(Sys.Date() -1,'%Y-%m-%d'))]

dt163 <- mysqlQuery(db = 'china_stocks',
                    query = "select TradingDay, stockID,
                                    open, high, low, close, chg, pchg,
                                    volume, turnover
                             from daily_from_163
                             where TradingDay >= 20040101
                             and close != 0")

source("~/myData/R/Rconfig/stocks.R")
source("~/myData/R/Rconfig/ip.R")
ipTables <- fetchIP(10)
ip <- ipTables[1]
print("Start fetchting...")
## -----------------------------------------------------------------------------

cl <- makeCluster(4, type = 'FORK')
parSapply(cl, 1:nrow(dt163[TradingDay >= '2018-01-01']), function(i){

    info <-dt163[i]

    ticker <- transform_sina_code(info$stockID)
    tradingYear <- substr(info$TradingDay, 1, 4)
    tradingDay <- gsub('-', '', info$TradingDay)
    destdir <-  paste0(csvPath, '/', tradingYear, '/', tradingDay)
    if (!dir.exists(destdir)) dir.create(destdir, recursive = T)
    destfile <- paste0(destdir, '/', info$stockID, '.csv')

    if (!file.exists(destfile)) {
        tryNo <- 0
        ## -------------------------------------------------------
        while (tryNo < 50) {
            tryNo <- tryNo + 1

            res <- fetchStockTransaction(ticker, info$TradingDay)
            #print(res)
            if (nrow(res) != 0) {
                fwrite(res, destfile)
                break
            }

        }
        ## -------------------------------------------------------
    }

})
stopCluster(cl)


# for (i in 1:nrow(dt163[TradingDay >= '2018-01-01'])) {
#     print(paste("## ", dt163[i, TradingDay], ":==>", dt163[i, stockID]))
#     info <-dt163[i]

#     ticker <- transform_sina_code(info$stockID)
#     tradingYear <- substr(info$TradingDay, 1, 4)
#     tradingDay <- gsub('-', '', info$TradingDay)
#     destdir <-  paste0(csvPath, '/', tradingYear, '/', tradingDay)
#     if (!dir.exists(destdir)) dir.create(destdir, recursive = T)
#     destfile <- paste0(destdir, '/', info$stockID, '.csv')

#     if (!file.exists(destfile)) {
#         tryNo <- 0
#         ## -------------------------------------------------------
#         while (tryNo < 50) {
#             tryNo <- tryNo + 1

#             res <- fetchStockTransaction(ticker, info$TradingDay)
#             #print(res)
#             if (nrow(res) != 0) {
#                 fwrite(res, destfile)
#                 break
#             }

#         }
#         ## -------------------------------------------------------
#     }

# }


