#! CiticPublic2mysql_40_NA_data.R

break_time_detector <- data.table(BreakBeginTime = c("21:00:00","00:00:00","09:00:00","13:00:00"),
                                  BreakEndTime   = c("23:59:59","02:30:00","11:30:00","15:15:00")
                                  )
break_time_detector[,':='(
  TradingDay   = logTradingDay
  ,DataSource = dataPath
  ,DataFile = logDataFile
)]

#-----------------------------------------------------------------------------
setcolorder(break_time_detector, c('TradingDay',"beginTime", "endTime",
                                   'DataSource','DataFile'))
#-----------------------------------------------------------------------------

################################################################################~~~~~~~~~~~~~~~~~~~~~~~~~~~
mysqlDBname <- 'CiticPublic'
mysql <- mysqlFetch(mysqlDBname)
# mysqlDBname <- paste(dbGetInfo(mysql)$dbname,
#                            gsub("/data/ChinaFuturesTickData/(.*)", "\\1", getwd()),
#                            sep = ".")

print("#---------- NO DATA WRITTEN INTO MySQL! --------------------------#")
#-------------------------------------------------------------------------------
dbWriteTable(mysql, "breakTime",
             break_time_detector, row.name = FALSE, append = T)
print("#---------- WRITTING break_time_detector into MySQL! -------------#")

dbDisconnect(mysql)
################################################################################
if(nrow(info) == 1 | nrow(dt) == 0){
  info <- data.table(status = "[错误提示]: 该文件没有数据.")
}
#-------------------------------------------------------------------------------
logInfo <- data.table(TradingDay  = logTradingDay
                       ,User       = Sys.info() %>% t() %>% data.table() %>% .[,user]
                       ,MysqlDB    = mysqlDBname
                       ,DataSource = dataPath
                       ,DataFile   = logDataFile
                       ,RscriptMain = logMainScript
                       ,RscriptSub = NA
                       ,ProgBeginTime = begin_time_marker
                       ,ProgEndTime   = Sys.time()
                       ,Results    = NA
                       ,Remarks    = NA
                       )

logInfo$RscriptSub  <- paste('(1) CiticPublic2mysql_01_read_data.R',
                              '             : (2) CiticPublic2mysql_02_manipulate_data.R',
                              '             : (3) CiticPublic2mysql_03_mysql_data.R',
                              '             : (4) CiticPublic2mysql_04_NA.R',
                              sep = " \n ")
logInfo$Results     <- paste(info$status,collapse = "\n ")
################################################################################~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
mysql <- mysqlFetch(mysqlDBname)
dbWriteTable(mysql, "log",
             logInfo, row.name = FALSE, append = T)
print("#---------- WRITTING processing log into MySQL! ------------------#")
#-------------------------------------------------------------------------------
dbDisconnect(mysql)
