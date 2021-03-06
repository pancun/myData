##! dt2MinuteBar.R
##
## 功能：
## 用于把 tick data 的数据转化为 分钟 的数据，
## 1. dt2MinuteBar(dt)
################################################################################
##------------------------------------------------------------------------------
dt2MinuteBar <- function(dt){
  setkey(dt,InstrumentID)
  temp <- lapply(unique(dt$InstrumentID), function(ii){ dt[ii] })

  no.cores <- max(round(detectCores()/4), 8)
  cl <- makeCluster(no.cores, type="FORK")
  dtMinute <- parLapply(cl, 1:length(temp), function(ii){
    ## -------------------------------------------------------------------------
    tempRes <- temp[[ii]] %>%
      .[, .SD[,.(
        #-----------------------------------------------------------------------
        NumericExchTime = .SD[1,NumericExchTime],
        #-----------------------------------------------------------------------
        OpenPrice  = .SD[DeltaVolume != 0][1,LastPrice],
        HighPrice  = .SD[DeltaVolume != 0, max(LastPrice, na.rm=TRUE)],
        LowPrice   = .SD[DeltaVolume != 0, min(LastPrice, na.rm=TRUE)],
        ClosePrice = ifelse(nrow(.SD[DeltaVolume != 0]) != 0,
                      .SD[DeltaVolume != 0][nrow(.SD[DeltaVolume != 0]), LastPrice],
                      .SD[.N,LastPrice]),
        #-----------------------------------------------------------------------
        Volume            = sum(.SD$DeltaVolume, na.rm=TRUE),
        Turnover          = sum(.SD$DeltaTurnover, na.rm=TRUE),
        #                 ------------------------------------------------------
        OpenOpenInterest  = .SD[1,OpenInterest],
        HighOpenInterest  = .SD[,max(OpenInterest, na.rm=TRUE)],
        LowOpenInterest   = .SD[,min(OpenInterest, na.rm=TRUE)],
        CloseOpenInterest = .SD[.N,OpenInterest],
        #                 ------------------------------------------------------
        UpperLimitPrice   = unique(na.omit(.SD$UpperLimitPrice)),
        LowerLimitPrice   = unique(na.omit(.SD$LowerLimitPrice)),
        SettlementPrice   = .SD[.N, SettlementPrice]
      )], by = .(TradingDay, InstrumentID, Minute)] %>%
      .[Volume != 0]
    return(tempRes)
    ## -------------------------------------------------------------------------
  }) %>% rbindlist()
  stopCluster(cl)
  return(dtMinute)
}
################################################################################
