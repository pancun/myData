################################################################################
## FundReporting.R
##
## 用于 基金交易 汇报
##
##
## 注意:
##
## Author: fl@hicloud-investment.com
## CreateDate: 2017-11-14
################################################################################

################################################################################
## STEP 0: 初始化，载入包，设定初始条件
################################################################################
rm(list = ls())

setwd('/home/fl/myData/')
suppressMessages({
  source('./R/Rconfig/myInit.R')
})

## =============================================================================
ChinaFuturesCalendar <- fread("./data/ChinaFuturesCalendar/ChinaFuturesCalendar.csv",
                              colClasses = list(character = c("nights","days")))
currTradingDay <- ChinaFuturesCalendar[days <= format(Sys.Date(), '%Y%m%d')][nights < format(Sys.Date(), '%Y%m%d')][.N]
## =============================================================================

accountAll <- data.table(
    accountID   = c('TianMi2','TianMi3','YunYang1','YunYang2'),
    accountName = c('甜蜜2号','甜蜜3号','云扬1号','云扬2号')
    )

logPath <- "/home/fl/myData/log/FundReporting"

tempFile <- paste0(logPath,'/',currTradingDay[1, days],'_fund.txt')
if (file.exists(tempFile)) file.remove(tempFile)
  tempFile <- paste0(logPath,'/',currTradingDay[1, days],'.txt')
if (file.exists(tempFile)) file.remove(tempFile)


## =============================================================================
## i = 1
fetchFund <- function(fundID, author = FALSE, host = '192.168.1.188') {
    ## ----------------------------
    dtNav <- mysqlQuery(db = 'posttrading',
                        query = sprintf("select * from nav
                                         where AccountID = '%s'
                                         and TradingDay = %s",
                                         fundID, currTradingDay[1, gsub('-', '', days)]),
                        host = host)
    if (nrow(dtNav) == 0) return(NA)
    ## ----------------------------
    funding <- mysqlQuery(db = 'posttrading',
                        query = sprintf("select * from funding
                                         where AccountID = '%s'
                                         and TradingDay = %s",
                                         fundID, currTradingDay[1, gsub('-', '', days)]),
                        host = host) %>% 
                        .[FundingClass == '赎回', ":="(
                            Amount = - Amount,
                            Shares = - Shares
                            )]

    netIncome <- ifelse(nrow(funding) != 0, funding[, sum(Amount)], 0)
    ## -------------------------------------------------------------------------
    fundPerformance <- data.table(
        基金名称 = dtNav[.N, AccountName],
        期货账户 = prettyNum(dtNav[.N, Futures], big.mark = ','),
        理财账户 = prettyNum(dtNav[.N, Stocks], big.mark = ','),
        账户总额 = prettyNum(dtNav[.N, Total], big.mark = ','),
        基金份额 = prettyNum(dtNav[.N, Shares], big.mark = ','),
        今日盈亏 = prettyNum(dtNav[.N, Profit] - ifelse(netIncome > 0, netIncome, 0),
                            big.mark = ','),
        收益波动 = paste0(ifelse(dtNav[.N, GrowthRatePct >= 0], '+', '-'),
                        dtNav[.N, abs(GrowthRatePct)], '%'),
        基金净值 = dtNav[.N, round(NetAssetValue,4)]
        )
    ## -------------------------------------------------------------------------

    mysql <- mysqlFetch(fundID, host)

    ## -------------------------------------------------------------------------
    positionInfo <- mysqlQuery(db = 'posttrading',
                               query = sprintf("select * from recordingPosition
                                                where AccountID = '%s'
                                                and TradingDay = %s",
                                                fundID, currTradingDay[1, gsub('-', '', days)]),
                               host = host)
    ## -------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    # tradingInfo <- dbGetQuery(mysql, paste(
    #     "select * from tradingInfo
    #     where TradingDay = ", currTradingDay[1, days])) %>%
    #     as.data.table() %>%
    #     .[, TradingDay := NULL]
    # print(tradingInfo)
    ## -------------------------------------------------------------------------


    ## -------------------------------------------------------------------------
    # failedInfo <- dbGetQuery(mysql,
    #     "select * from failedInfo") %>% as.data.table()
    # failedInfo[offset == '平仓', direction := ifelse(direction == 'long','short','short')]
    # failedInfo[, offset := NULL]
    # setcolorder(failedInfo, c('TradingDay', 'strategyID', 'InstrumentID', 'direction','volume'))
    # print(positionInfo)
    ## -------------------------------------------------------------------------

    ## ---------------------------------------------------------------------------
    ## 写入 log
    tempFile <- paste0(logPath,'/',currTradingDay[1, days],'_fund.txt')
    if (!grepl('SimNow', accountAll[i,accountID])) {
        sink(tempFile, append = TRUE)
        cat("## ----------------------------------- ##")
        cat('\n')
        write.table(as.data.frame(t(fundPerformance)), tempFile
                    , append = TRUE, col.names = FALSE
                    , sep = ' :==> ')
        # if (!is.na(grep('SimNow', accountAll$accountID)[1])) {
        #     if (i == grep('SimNow', accountAll$accountID)[1]-1) {
        #         cat("## ----------------------------------- ##\n")
        #         cat("\n## ----------------------------------- ##\n")
        #         cat("## @william")
        #     }
        # }

        if (author) {
            cat("\n## ----------------------------------- ##\n")
            cat("## @william")
        }
    }

    ## ===========================================================================
    tempFile <- paste0(logPath,'/',currTradingDay[1, days],'.txt')
    sink(tempFile, append = TRUE)
    cat("## ----------------------------------- ##")
    cat('\n')
    cat(paste0('## >>>>>>>>> ',fundID))
    cat('\n')
    cat("## ----------------------------------- ##")
    cat('\n')
    write.table(as.data.frame(t(fundPerformance)), tempFile
                , append = TRUE, col.names = FALSE
                , sep = ' :==> ')
    cat('\n')
    if (nrow(positionInfo) != 0) {
        cat("## ----------------------------------- ##")
        cat("\n## 今日持仓信息 ##")
        cat('\n')
        # tmp <- positionInfo[, .(InstrumentID,InstrumentName,
        #                        Direction, Volume, TotalProfit)]
        # colnames(tmp) <- c('合约代码','合约名称','持仓方向','持仓手数','持仓盈亏')
        tmp <- positionInfo[, .(InstrumentID,
                               Direction, Volume, TotalProfit)]
        colnames(tmp) <- c('合约代码','持仓方向','持仓手数','持仓盈亏')
        print(tmp)
        cat('\n')
    }  

    # if (nrow(failedInfo) != 0) {
    #     cat("## ----------------------------------- ##")
    #     cat("\n## 未平仓信息 ##")
    #     cat('\n')
    #     print(failedInfo)
    #     cat('\n')
    # }

    ## ===========================================================================
    ## 不看交易记录
    # if (nrow(tradingInfo) != 0) {
    #   write.table("## -------------------------------------- ##", tempFile
    #               , append = TRUE, col.names = FALSE, row.names = FALSE)
    #   cat("## 今日交易记录 ##")
    #   cat('\n')
    #   print(tradingInfo)
    #   cat('\n')
    # }
    cat("## ----------------------------------- ##\n")
    ## ===========================================================================
    if (author) {
        cat("\n## ----------------------------------- ##\n")
        cat("## @william")
    }
}
## =============================================================================

for (i in 1:nrow(accountAll)) {
    # print(i)
    id <- accountAll[i, accountID]

    if (i < nrow(accountAll)) {
        fetchFund(accountAll[i, accountID])
    } else {
        fetchFund(accountAll[i, accountID], author = TRUE)
    }
}
## ----------------------------------------
for ( conns in dbListConnections(MySQL()) ) dbDisconnect(conns)
## ----------------------------------------
