#! citic2mysql_03_mysql.R
#
################################################################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## =============================================================================
## dtTick 写入数据库
mysql <- mysqlFetch('CTPMD')
mysqlDBname <- paste(dbGetInfo(mysql)$dbname,
                           coloID[k,colo],
                           sep = ".")
dbSendQuery(mysql,paste0("DELETE FROM tick
            WHERE TradingDay = ", logTradingDay))
dbWriteTable(mysql, "tick",
             dtTick, row.name = FALSE, append = T)
dbDisconnect(mysql)
## =============================================================================


## =============================================================================
## 写入 Bar 数据
mysql <- mysqlFetch('CTPMD')

## Delete bar data
dbSendQuery(mysql,paste0("DELETE FROM minute
            WHERE TradingDay = ", logTradingDay))
dbSendQuery(mysql,paste0("DELETE FROM daily
            WHERE TradingDay = ", logTradingDay))

dbWriteTable(mysql,"minute",
             dtMinute, row.name　=　FALSE, append = T)

## -- DailyBar
dbWriteTable(mysql, "daily",
             dt_allday, row.name　=　FALSE, append = T)
## -- DayBar
dbWriteTable(mysql, "daily",
             dt_day, row.name　=　FALSE, append = T)

## -- NightBar
if(nrow(dt_night) != 0){  #--- 如果非空，则录入 MySQL 数据库
  dbWriteTable(mysql, "daily",
               dt_night, row.name　=　FALSE, append = T)
}

print("#---------- WRITTING DATA INTO MySQL! ----------------------------#")
dbDisconnect(mysql)
## =============================================================================


## =============================================================================
if(nrow(info) == 1){
  info[1, status := "[错误提示]: 该文件没有数据."]
}
#-------------------------------------------------------------------------------
logInfo <- data.table(TradingDay  = logTradingDay
                       ,User       = Sys.info() %>% t() %>% data.table() %>% .[,user]
                       ,MysqlDB    = mysqlDBname
                       ,DataSource = coloID[k,colo]
                       ,DataFile   = logDataFile
                       ,RscriptMain = logMainScript
                       ,RscriptSub = NA
                       ,ProgBeginTime = logBeginTime
                       ,ProgEndTime   = Sys.time()
                       ,Results    = NA
                       ,Remarks    = NA
)

logInfo$RscriptSub  <- paste('(1) ctpMD2mysql_01_read_data.R',
                              '            : (2) ctpMD2mysql_02_manipulate_data.R',
                              '            : (3) ctpMD2mysql_03_mysql_data.R',
                              '            : (4) ctpMD2mysql_04_NA.R',
                              sep = " \n ")
logInfo$Results     <- paste(info$status,collapse = "\n ")
## =============================================================================


## =============================================================================
mysql <- mysqlFetch('CTPMD')

if(exists('break_time_detector')){
  dbWriteTable(mysql,'breakTime',
               break_time_detector, row.name=FALSE, append = T)
}
print("#---------- WRITTING break_time_detector into MySQL! -------------#")

if (tempHour %between% c(8,19) | includeHistory) {
  dbWriteTable(mysql, "log",
             logInfo, row.name = FALSE, append = T)
  print("#---------- WRITTING processing log into MySQL! ------------------#")
}

## =============================================================================

################################################################################
dbDisconnect(mysql)
for(conn in dbListConnections(MySQL()) )
  dbDisconnect(conn)
################################################################################
