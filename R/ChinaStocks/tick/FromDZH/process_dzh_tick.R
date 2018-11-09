## =============================================================================
## @Author: “williamlfang”
## @Date:   2018-11-06 20:05:16
## @Last Modified by: “williamlfang”
## @Last Modified at: 2018-11-09 16:09:32
## 
## 处理大智慧 Tick 股票数据
## =============================================================================

rm(list = ls())

## =============================================================================
setwd('/home/fl/myData/')
suppressMessages({
  source('./R/Rconfig/myInit.R')
  source('./R/Rconfig/stocks.R')
})
library(fst)

DATA_PATH <- '/data/ChinaStocks/TickData/FromDZH'
fstPath <- '/data/ChinaStocks/TickData/FromDZHfst'
logPath <- '/home/fl/myData/log/ChinaStocks/FromDZH'
## =============================================================================


## =============================================================================
## 读取大智慧 Tick 股票分笔数据
## @params:
##      - datafile： 数据文件
##      - exchID: 交易所标示
## ----------------------------
readDzhTick <- function(datafile, exchID) {
    stockID <- gsub('.*([0-9]{6})\\.txt$', '\\1', datafile)
    
    tick <- readTick(datafile, encoding = 'GB18030')
    tick[, ":="(
        时间 = sprintf("%06d", 时间)
        ,代码 = stockID
        ,交易所 = exchID
        )]
    tick[, ":="(
        时 = substr(时间, 1, 2)
        ,分 = substr(时间, 3, 4)
        ,秒 = substr(时间, 5, 6)
        )]
    setcolorder(tick,
                c('日期','代码', '交易所',
                  colnames(tick)[!colnames(tick) %in% c('日期','代码','交易所')]
                )
    )
    
    return(tick)
}

# f <- "/data/ChinaStocks/TickData/FromDZH/2015/sh/000001.txt"
# tmp <- readDzhTick(f, 'sh')
## =============================================================================


## =============================================================================
## 2009 - 2017 是完整的一份数据
## --------------------------

allYears <- dir(DATA_PATH) %>% 
    .[as.numeric(.) %between% c(2009, 2017)]

## -----------------------------------------
args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 1) allYears <- args[1]
## -----------------------------------------

for (yr in allYears) {
    sink(sprintf("/home/fl/myLog/process_dzh_tick_%s.txt", yr), append = TRUE)

    ## -------------------------------------------------------------------------
    destdir <- sprintf("%s/%s", fstPath, yr)
    if (!dir.exists(destdir)) dir.create(destdir)

    for (exchID in c('sh','sz')) {
        ## print(sprintf("## %s :==> %s :==> @ %s", yr, exchID, Sys.time()))

        allFiles <- sprintf('%s/%s/%s', DATA_PATH, yr, exchID) %>% 
            list.files(., pattern = '\\.txt$', full.names = TRUE)

        ## ---------------------------------------------------------------------
        for (f in allFiles) {
            stockID <- gsub('.*([0-9]{6})\\.txt$', '\\1', f)
            destfile <- sprintf("%s/%s%s.fst", destdir, exchID, stockID)

            if (! file.exists(destfile)) {
                print(sprintf("## %s :==> %s :==> %s :==> @%s", 
                              yr, exchID, f, Sys.time()))
                res <- readDzhTick(f, exchID)
                fst::write_fst(res, destfile, compress = 100)
            }
        }
        ## ---------------------------------------------------------------------

    }
    ## -------------------------------------------------------------------------
    sink(NULL)
}


## =============================================================================


## =============================================================================
## 检查 NA
## log_na <- sprintf('%s/log_na.csv', logPath)
## =============================================================================
