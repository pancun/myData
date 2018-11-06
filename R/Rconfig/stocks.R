Rcpp::sourceCpp("/home/fl/myData/R/Rconfig/myRcpp.cpp")

## =============================================================================
## fast reading Sina Transactions Data
## @param:  - datafile: a csv file
## @return: - a data.table
options(datatable.integer64 = "numeric")
## Ref: https://stackoverflow.com/questions/18718045/datatable-integer64-argument-is-not-working-for-me-should-it
readSinaTick <- function(datafile) {

    tmp <- readRcpp(datafile)

    if (!grepl('买盘|卖盘', tmp)) {
        tmp <- iconv(tmp, from = 'GB18030', to = 'utf8')
    }
    if (is.na(tmp)) return(data.table(成交时间 = NA))

    res <- data.table::fread(tmp, fill = TRUE)

    return(res)
}
## =============================================================================


## =============================================================================
transform_wind_code <- function(stockID) {
    if (substr(stockID, 1, 1) == '6') {
        res <- paste0(stockID, ".SH")
    } else {
        res <- paste0(stockID, ".SZ")
    }
    return(res)
}

transform_sina_code <- function(stockID) {
    if (substr(stockID, 1, 1) == '6') {
        res <- "sh"
    } else {
        res <- "sz"
    }
    return(paste0(res, stockID))
}
## =============================================================================


## =============================================================================
## 清理股票名称
## 把原来等宽的中文字体换成 utf8
## ------------------------
cleanStockName <- function(stockName) {
 res <- gsub(' ', '', stockName) %>%
        gsub('Ａ', 'A', .) %>%
        gsub("Ｂ", "B", .)
}
## =============================================================================


## =============================================================================
## 获取 上海交易所 深圳交易所 上市公司股票代码与名称
## ---------------------------------------
url_sse_listing <- "http://query.sse.com.cn/security/stock/getStockListData2.do"
headers_sse_listing <- c(
    "Accept"          = "*/*",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,en-US;q=0.8,zh;q=0.6,en;q=0.4,zh-TW;q=0.2",
    "Connection"      = "keep-alive",
    "DNT"             = "1",
    "Host"            = "query.sse.com.cn",
    "Referer"         = "http://www.sse.com.cn/assortment/stock/list/share/",
    "User-Agent"      = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
    )
query_sse_listing <- list(
    isPagination       = "true",
    stockCode          = "",
    csrcCode           = "",
    areaName           = "",
    stockType          = "1",
    ## ----------------------
    pageHelp.cacheSize = "1",
    pageHelp.beginPage = "1",
    pageHelp.pageSize  = "5000", ## 最大到 5000
    pageHelp.pageNo    = "1",
    pageHelp.endPage   = "21")
fetch_sse_listing <- function() {
    ## 获取服务器资源
    tryNo <- 0
    while (tryNo < 30) {
        if (tryNo == 30) {
            print("上交所 股票列表 下载失败.")
            return(data.table())
        }
        tryNo <- tryNo + 1

        if (class(try(
                      r <- GET(url_sse_listing, query = query_sse_listing,
                               add_headers(headers_sse_listing),
                               timeout(30))
                      , silent = TRUE)) != 'try-error') break
    }

    ## 开始解析网页，可以直接使用 json 各式进行转化
    page <- content(r, 'text')
    jsonFile <- rjson::fromJSON(page)
    # summary(jsonFile)

    # jsonFile$pageHelp$pageCount
    #                  Length Class  Mode
    # areaName          1     -none- character
    # csrcCode          1     -none- character
    # downloadFileName  0     -none- NULL
    # execlStream       0     -none- NULL
    # jsonCallBack      0     -none- NULL
    # pageHelp         13     -none- list
    # result           25     -none- list
    # stockCode         1     -none- character
    # stockType         1     -none- character

    temp <- jsonFile$result
    dt <- lapply(1:length(temp), function(i){
        as.data.table(temp[[i]])
    }) %>% rbindlist()
    # print(dt)

    ## 选择需要的字段
    dt <- dt[, .(stockID = COMPANY_CODE, stockName = COMPANY_ABBR,
                 stockID_B = SECURITY_CODE_B, stockName_B = SECURITY_ABBR_B,
                 listingDate = LISTING_DATE,
                 exchID = 'sh'
               )]

    ## B股如果没有，则表示为 NA
    dt[stockName_B == '-',
        ":="(stockID_B = NA,
             stockName_B = NA
            )
    ]

    ## 清理 A/B 之母问题
    dt[, ":="(
        stockName = cleanStockName(stockName),
        stockName_B = cleanStockName(stockName_B)
    )]

    ## 600996 上交所有问题,没有更新 IPO 上市时间
    dt[stockID == '600996', listingDate := '2016-12-26']

    return(dt)
}

##
url_szse_listing_A <- "http://www.szse.cn/szseWeb/ShowReport.szse?SHOWTYPE=xlsx&CATALOGID=1110&tab2PAGENO=1&ENCODE=1&TABKEY=tab2"
##
url_szse_listing_B <- "http://www.szse.cn/szseWeb/ShowReport.szse?SHOWTYPE=xlsx&CATALOGID=1110&tab3PAGENO=1&ENCODE=1&TABKEY=tab3"

fetch_szse_listing <- function() {
    ## 尝试把以前的文件先删除掉
    fa <- '/tmp/szA.xlsx'
    fb <- '/tmp/szB.xlsx'
    try(file.remove(fa), silent = T)
    try(file.remove(fb), silent = T)

    ## -------------------------------------------------------------------------
    ## 下载 深交所 A股数据
    tryNo <- 0
    while (tryNo <= 10) {
        # print(tryNo)
        tryNo <- tryNo + 1
        if (class(try(
            GET(url_szse_listing_A,
                write_disk(fa, overwrite = TRUE),
                timeout(60))
          , silent = T)) != 'try-error') break
    }
    if (file.exists(fa)) {
        dtA <- readxl::read_excel(fa) %>%
            as.data.table() %>%
            .[, .(stockID = 公司代码,
                  stockName = 公司简称,
                  listingDate = A股上市日期,
                  exchID = 'sz')]
    } else {
        return(data.table())
    }

    ## -------------------------------------------------------------------------
    ## 下载 深交所 B股数据
    tryNo <- 0
    while (tryNo <= 10) {
        # print(tryNo)
        tryNo <- tryNo + 1
        if (class(try(
            GET(url_szse_listing_B,
                write_disk(fb, overwrite = TRUE),
                timeout(60))
          , silent = T)) != 'try-error') break
    }
    if (file.exists(fb)) {
        dtB <- readxl::read_excel(fb) %>%
            as.data.table() %>%
            .[, .(stockID = 公司代码,
                  stockName = 公司简称,
                  stockID_B = B股代码,
                  stockName_B = B股简称)]
    } else {
        return(data.table())
    }

    ## -------------------------------------------------------------------------
    dt <- merge(dtA, dtB, by = c('stockID','stockName'), all.x = TRUE)
    dt[, ":="(
      stockName = cleanStockName(stockName),
      stockName_B = cleanStockName(stockName_B)
    )]

    return(dt)
}
## =============================================================================


## =============================================================================
## 从上交所、深交所下载股票停复牌公告数据
##
## - fetch_sse_suspension：上交所：[披露 -> 交易提示 -> 停复牌信息](http://www.sse.com.cn/disclosure/dealinstruc/suspension/)
## - fetch_szse_suspension：深交所：[信息披露 -> 交易备忘](http://www.szse.cn/disclosure/memo/index.html)
## ----------------------------------

url_sse_suspension <- "http://query.sse.com.cn/infodisplay/querySpecialTipsInfoByPage.do"
headers_sse_suspension <- c(
    "Accept"          = "*/*",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,en-US;q=0.8,zh;q=0.6,en;q=0.4,zh-TW;q=0.2",
    "Connection"      = "keep-alive",
    "DNT"             = "1",
    "Host"            = "query.sse.com.cn",
    "Referer"         = "http://www.sse.com.cn/assortment/stock/list/share/",
    "User-Agent"      = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/60.0.3112.113 Safari/537.36"
    )
query_sse_suspension <- list(
    isPagination       = "true"
    ,searchDate         = "2018-11-05"
    ,bgFlag             = "1"
    ,searchDo           = "1"
    ## ----------------------
    # ,pageHelp.cacheSize = "1"
    # ,pageHelp.beginPage = "1"
    ,pageHelp.pageSize  = "10000" ## 最大到 5000
    # ,pageHelp.pageNo    = "1"
    # ,pageHelp.endPage   = "5"
    )

fetch_sse_suspension <- function (tdate) {
    query_sse_suspension$searchDate <- ymd(tdate)

    tryNo <- 0
    while (tryNo < 10) {
        tryNo <- tryNo + 1

        if (class(try(
                r <- GET(url_sse_suspension, query = query_sse_suspension,
                       add_headers(headers_sse_suspension),
                       timeout(60))
                , silent = TRUE)) == 'try-error') {
            res <- data.table()
            next
        }

        if (tryNo == 30) {
            res <- data.table()
            break
        }

        p <- content(r, 'parsed')

        ## ---------------------------------------------------------------------
        if (length(p$pageHelp$data) == 0) {
            res <- data.table(
                stockID = NA, stockName = NA,
                suspensionStart = NA, suspensionStop = NA,
                suspensionTime = NA, suspensionReason = NA)
        } else {
            res <- lapply(1:length(p$pageHelp$data), function(i){
                data.table(t(p$pageHelp$data[[i]]))
            }) %>% rbindlist() %>%
                .[, .(stockID = productCode, stockName = productName,
                    suspensionStart = showDate, suspensionStop = stopDate,
                    suspensionTime = stopTime, suspensionReason = stopReason)
                ]
            ## 防止出现 NULL 的情况
            cols <- colnames(res)
            res[, (cols) := lapply(.SD, as.character), .SDcols = cols]
        }
        ## ---------------------------------------------------------------------
        break
    }

    return(res)
}
# tdate <- '2018-11-01'
# tmp.test <- fetch_sse_suspension(tdate)


url_szse_suspension <- "http://www.szse.cn/api/report/ShowReport/data"
headers_szse_suspension <- c(
    'Accept' = 'application/json, text/javascript, */*; q=0.01'
    ,'Accept-Encoding' = 'gzip, deflate'
    ,'Accept-Language' = 'zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6'
    ,'Connection' = 'keep-alive'
    ,'Content-Type' = 'application/json'
    ,'DNT' = '1'
    ,'Host' = 'www.szse.cn'
    ,'Referer' = 'http://www.szse.cn/disclosure/memo/index.html'
    ,'User-Agent' = 'Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36'
    ,'X-Request-Type' = 'ajax'
    ,'X-Requested-With' = 'XMLHttpRequest'
    )

query_szse_suspension <- list(
    'SHOWTYPE' = 'JSON'
    ,'CATALOGID' = '1798'
    ,'TABKEY' = 'tab1'
    ,'txtKsrq' = '2007-01-01'
    ,'txtZzrq' = format(Sys.Date(), '%Y-%m-%d')
    ,'txtKsrq-txtZzrq' = format(Sys.Date(), '%Y-%m-%d')
    )

fetch_szse_suspension <- function (fromDate = '2007-01-01', toDate = format(Sys.Date(), '%Y-%m-%d')) {
    ## 第一步，先获取一共有多少页数据 ==> allPages
    query_szse_suspension$txtKsrq <- fromDate
    query_szse_suspension$txtZzrq <- toDate

    if (class(try(
            r <- GET(url_szse_suspension,
                     query = query_szse_suspension,
                     add_headers(headers_szse_suspension),
                     timeout(30))
                  , silent = TRUE)) == 'try-error') return(data.table())

    allPages <- content(r, 'parsed') %>% .[[1]] %>%
        .$metadata %>%
        .$pagecount %>%
        as.numeric()

    dt <- list()

    for (i in 1:allPages) {
        print(sprintf("## %s :==> @%s", i, Sys.time()))

        query_szse_suspension$PAGENO <- i

        tryNo <- 0
        while (tryNo <= 30) {
            if (tryNo == 30) {
                return(data.table())
            }
            tryNo <- tryNo + 1

            if (class(try(
                    r <- GET(url_szse_suspension,
                            query = query_szse_suspension,
                            add_headers(headers_szse_suspension),
                            timeout(20))
                        ,silent = TRUE)) == 'try-error') {
                res <- data.table()
                next
            }

            p <- content(r, 'parsed', encoding = 'GB18030')
            d <- p[[1]]$data
            res <- lapply(d, as.data.table) %>% rbindlist()
            ## ------------------
            if (nrow(res) != 0) {
                res[, zqjc := gsub('&nbsp;', '', zqjc)]
                dt[[i]] <- res
                break
            }
            ## ------------------
        }

    }

    return(rbindlist(dt))
}

# fromDate <- Sys.Date() - 15
# tmp.test <- fetch_szse_suspension(fromDate)
## =============================================================================



## =============================================================================
## 从新浪下载股票成交明细数据
## --------------------------
HEADERS_SINA_MARKET <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Connection" = "keep-alive",
    "DNT" = "1",
    "Host" = "market.finance.sina.com.cn",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36"
)
URL_SINA_MARKET <- "http://market.finance.sina.com.cn/transHis.php"

fetchPageTransaction <- function(stockID, tradingDay, page, ip) {
    # stockID <- 'sz000001'
    # tradingDay <- '2018-10-12'

    tday <- ymd(tradingDay)

    q <- list(
        symbol = stockID,
        date   = tday,
        page   = as.character(page))

    if (class(try(
        r <- GET(URL_SINA_MARKET
                 ,query = q
                 ,add_headers(HEADERS_SINA_MARKET)
                 ,timeout(10)
                 ,use_proxy(ip[1, ip], ip[1, as.numeric(port)])
        )
        ,silent = T
    )
    ) == 'try-error') {
        return(data.table())
    }

    ## 如果链接错误
    if (r$status_code != 200 |
        is.na(content(r, as = 'text', encoding = 'GB18030'))) {
        return(NA)
    } else {
        if (is.null(r$header$server)) {
            return(data.table())
        } else if (r$headers$server != 'Sina') {
            return(data.table())
        }
    }

    # Sys.sleep(.1)

    p <- content(r, 'parsed', encoding = 'GB18030')

    res <- p %>%
        html_nodes('table') %>%
        html_table() %>%
        rbindlist()

    ## 如果数据为空
    ## 说明有错误
    if (nrow(res) == 0) {
        return(data.table())
    }

    return(res)
}

fetchStockTransaction <- function(stockID, tradingDay) {
    # stockID <- 'sz000001'
    # tradingDay <- '2018-10-12'

    finalData <- FALSE

    dt <- list()

    for (i in 1:50) {
        print(i)

        finalTryNo <- 50
        for (j in 1:finalTryNo) {
            res <- fetchPageTransaction(stockID, tradingDay, i, ip)

            ## 如果是 NA
            ## 说明 IP 不能用了
            if (any(is.na(res))) {
                tmp <- ip[1, ip]

                ipTables[ip == tmp, tryNo := tryNo + 1]
                ipTables <- ipTables[tryNo < 500]

                if (nrow(ipTables) < 5) {
                    ipTables <- fetchIP(10, core = 4)
                }

                ip <- ipTables[ip != tmp] %>% .[sample(1:.N,1)]
                next
            }

            ## 如果是正常的数据
            ## 就说明获得了正确的数据
            ## 然后退出这个循环
            if (nrow(res) != 0) {
                dt[[i]] <- res
                break
            }

            ## ------------------------------
            if (j == finalTryNo) {
                if (is.null(nrow(res)) | nrow(res) == 0) {
                    dt[[i]] <- NA
                }
            }
            ## ------------------------------
        }

        if (is.na(dt[[i]])) return(data.table())

        ## 最后一行
        if (nrow(dt[[i]]) == 1) {
            finalData <- TRUE
            break
        }

    }

    if (finalData) {
        dt <- rbindlist(dt) %>%
            .[!duplicated(.)]
    } else {
        dt <- data.table()
    }

    return(dt)
}

## =============================================================================
if (F) {
    source("~/myData/R/Rconfig/ip.R")
    ipTables <- fetchIP(1)
    ip <- ipTables[1]

    dt <- fetchStockTransaction('sz000002', '2018-10-11')
    print(dt)

    dt <- fetchStockTransaction('sz000005', '2016-01-04')
    print(dt)
}

## =============================================================================

