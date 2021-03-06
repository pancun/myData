################################################################################
##! ctpMD2mysql_01_read_data.R
## 这是次函数:
## 用于读取 CTPMD1 的 csv 文件
##
## 包括:
##
##
##
## 注意:
##
## Author: fl@hicloud-investment.com
## CreateDate: 2017-01-16
## UpdateDate: 2017-07-18
################################################################################
source('./R/Rconfig/myDay.R')
if(currTradingDay[i,nchar(nights) == 0]){
  ##-- 如果没有夜盘的话，则需要去掉 myDay
  myDay <- myDay[trading_period %between% c("08:00:00", "16:00:00")]
  myDayPlus <- myDayPlus[trading_period %between% c("08:00:00", "16:00:00")]
}

if (! identical(dataFileNight, character(0)) ){
    dtNight <- myFreadBarCTP(paste(dataPath, dataFileNight, sep = '/'))
} else {
    dtNight <- data.table()
}

if (! identical(dataFileDay, character(0)) ){
    dtDay <- myFreadBarCTP(paste(dataPath, dataFileDay, sep = '/'))
} else {
    dtDay <- data.table()
}

dt <- list(dtNight, dtDay) %>% rbindlist()

info <- data.table(status = paste("(1) [读入数据]: 原始数据                                :==> Rows:", nrow(dt),
                              "/ Columns:", ncol(dt), sep=" ")
               )
################################################################################



