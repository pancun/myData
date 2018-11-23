## =============================================================================
## 获取可用的免费代理 IP
##
## DATE: 2018-10-09 13:56:08
## AUTHOR: william
## =============================================================================

## =============================================================================
library(magrittr)
library(data.table)
library(httr)
library(rvest)
library(parallel)
## =============================================================================


## =============================================================================
## 快代理
# https://www.kuaidaili.com/free/

h_kuaidaili <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate, br",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Cache-Control" = "max-age=0",
    "Connection" = "keep-alive",
    "Cookie" = "channelid=0; sid=1539065692194656; _ga=GA1.2.48158171.1539065694; _gid=GA1.2.267452307.1539065694",
    "DNT" = "1",
    "Host" = "www.kuaidaili.com",
    "Referer" = "https://www.kuaidaili.com/free/inha/2/",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36"
    )

fetch_kuaidaili <- function(n = 5) {
    print("## fetch_kuaidaili...")
    kuaidaili <- lapply(1:n, function(i){
        url <- sprintf("https://www.kuaidaili.com/free/inha/%s/",i)

        if (class(try(
                      r <- GET(url, add_headers(h_kuaidaili))
                      ,silent = T)) == 'try-error') return(data.table())

        Sys.sleep(1)

        if (r$status_code != 200) return(data.table())

        res <- content(r) %>%
            html_nodes("table") %>%
            html_table() %>%
            rbindlist()

        }) %>% rbindlist()

    if (nrow(kuaidaili) != 0) {
        kuaidaili <- kuaidaili[, .(ip = IP, port = PORT)] %>%
                    .[!duplicated(.)]
    } else {
        kuaidaili <- data.table()
    }

    return(kuaidaili)
}
## =============================================================================

## =============================================================================
## 无忧代理
# http://www.data5u.com/free/index.html
h_wuyou <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Cache-Control" = "max-age=0",
    "Connection" = "keep-alive",
    "DNT" = "1",
    "Host" = "www.data5u.com",
    "Referer" = "http://www.data5u.com/api/create-dynamic.html",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36")
url <- c('http://www.data5u.com/free/gngn/index.shtml',
         'http://www.data5u.com/free/index.html',
         'http://www.data5u.com/free/gnpt/index.shtml',
         'http://www.data5u.com/free/gwgn/index.shtml',
         'http://www.data5u.com/free/gwpt/index.shtml')

fetch_wuyou <- function() {
    print("## fetch_wuyou...")
    wuyou <- lapply(url, function(u){
        if (class(try(
                    r <- GET(u, add_headers(h_wuyou))
                    ,silent = T)) == 'try-error') return(data.table())

        Sys.sleep(1)
        if (r$status_code != 200) return(data.table())

        res <- content(r) %>%
            html_nodes(".wlist .l2") %>%
            html_text() %>%
            lapply(., function(x){
                tmp <- strsplit(x, '\r\n\t\t') %>% unlist() %>%
                    gsub(' ', '', .)
                return(data.table(ip = tmp[1],
                                  port = tmp[2]
                                  )
                        )
            }) %>% rbindlist()
    }) %>% rbindlist() %>%
    .[!duplicated(.)]

    return(wuyou)
}

## =============================================================================


## =============================================================================
## http://ip.jiangxianli.com/
h_jiangxianli <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Cache-Control" = "max-age=0",
    "Connection" = "keep-alive",
    "DNT" = "1",
    "Host" = "ip.jiangxianli.com",
    "Referer" = "http://ip.jiangxianli.com/",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36"
    )

fetch_jiangxianli <- function(n = 10) {
    print("## fetch_jiangxianli...")
    jiangxianli <- lapply(1:n, function(i){
        if (class(try(
                    r <- GET(url = "http://ip.jiangxianli.com/",
                             query = list(page = i),
                             add_headers(h_jiangxianli)
                             )
                    ,silent = T)) == 'try-error') return(data.table())

        if (r$status_code != 200) return(data.table())

        res <- content(r) %>%
            html_nodes('table') %>%
            html_table() %>%
            rbindlist() %>%
            .[, .(ip = IP, port = 端口)]
    }) %>% rbindlist() %>%
    .[!duplicated(.)]

    return(jiangxianli)
}
## =============================================================================


## =============================================================================
## http://lab.crossincode.com/proxy/
h_crossincode <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Cache-Control" = "max-age=0",
    "Connection" = "keep-alive",
    "Cookie" = "pgv_pvi=324861952; pgv_si=s3735448576",
    "DNT" = "1",
    "Host" = "lab.crossincode.com",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36"
       )
fetch_crossincode <- function(){
    print("## fetch_crossincode...")

    if (class(try(
                r <- GET("http://lab.crossincode.com/proxy/",
                         add_headers(h_crossincode))
                ,silent = T)) == 'try-error') return(data.table())


    if (r$status_code != 200) return(data.table())

    crossincode <- content(r) %>%
        html_nodes('table') %>%
        html_table() %>%
        rbindlist() %>%
        .[, .(ip = Addr, port = Port)] %>%
        .[!duplicated(.)]

    return(crossincode)
}
## =============================================================================


## =============================================================================
## 云代理
# http://www.ip3366.net
h_yundaili <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Connection" = "keep-alive",
    "DNT" = "1",
    "Host" = "www.ip3366.net",
    "Referer" = "http://www.ip3366.net/free/",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36")

fetch_yundaili <- function(n = 10) {
    print("## fetch_yundaili...")

    yundaili <- lapply(1:n, function(i){
        if (class(try(
                    r <- GET(url = "http://www.ip3366.net/free/",
                             query = list(stype = 1,
                                          page = i),
                             add_headers(h_yundaili))
                    ,silent = T)) == 'try-error') return(data.table())

        if (r$status_code != 200) return(data.table())

        res <- content(r, encoding = 'gb2312') %>%
            html_nodes('table') %>%
            html_table() %>%
            rbindlist() %>%
            .[, .(ip = IP, port = PORT)]
    }) %>% rbindlist() %>%
    .[!duplicated(.)]

    return(yundaili)
}

## =============================================================================


## =============================================================================
## 66ip
h_66ip <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Connection" = "keep-alive",
    "DNT" = "1",
    "Host" = "www.66ip.cn",
    "Referer" = "http://www.66ip.cn/",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36"
    )

fetch_66ip <- function() {
    print("## fetch_66ip...")

    url <- "http://www.66ip.cn/nmtq.php"
    if (class(try(
                    r <- GET(url, add_headers(h_66ip),
                             query = list(
                                          getnum = 500,
                                          isp = 0,
                                          proxytype = 2,
                                          api = '66ip'
                                          )
                             )
                  ,silent = T)) == 'try-error') return(data.table())

    if (r$status_code != 200) return(data.table())

    p <- content(r, 'text', encoding = "GB18030")
    if (is.na(p)) return(data.table())

    ip <- read_html(p) %>%
        html_nodes('p') %>%
        html_text() %>%
        strsplit(., "\r\n\\t") %>% unlist() %>%
        .[-c(1:2, length(.))] %>%
        gsub('\\t', '', .)

    ip66 <- lapply(1:length(ip), function(i){
            temp <- unlist(strsplit(ip[i],":"))
            data.table(url = temp[1], port = temp[2])
        }) %>% rbindlist() %>%
        .[!duplicated(.)]

    return(ip66)
}
## =============================================================================


## =============================================================================
## 飞蚁代理
# http://www.feiyiproxy.com/?page_id=1457
h_feiyidaili <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Cache-Control" = "max-age=0",
    "Connection" = "keep-alive",
    "DNT" = "1",
    "Host" = "www.feiyiproxy.com",
    "Referer" = "http://www.feiyiproxy.com/",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36"
    )

fetch_feiyidaili <- function() {
    print("## fetch_feiyidaili...")
    if (class(try(
                r <- GET("http://www.feiyiproxy.com/?page_id=1457",
                         add_headers(h_feiyidaili)
                         )
                , silent = T)) == 'try-error') return(data.table())

    if (r$status_code != 200) return(data.table())

    feiyidaili <- content(r) %>%
        html_nodes('table') %>%
        html_table() %>%
        .[[1]] %>%
        as.data.table() %>%
        .[, .(ip = IP, port = 端口)]

    return(feiyidaili)
}
## =============================================================================


## =============================================================================
## goubanjia
# http://www.goubanjia.com/
h_goubanjia <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Cache-Control" = "max-age=0",
    "Connection" = "keep-alive",
    "DNT" = "1",
    "Host" = "www.goubanjia.com",
    "Referer" = "http://www.goubanjia.com/",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36"
    )

fetch_goubanjia <- function() {
    print("## fetch_goubanjia...")

    if (class(try(
                r <- GET("http://www.goubanjia.com/",
                         add_headers(h_goubanjia)
                         )
                ,silent = T)) == 'try-error') return(data.table())

    if (r$status_code != 200) return(data.table())

    res <- content(r) %>%
        html_nodes('table') %>%
        html_table() %>%
        rbindlist()
    colnames(res) <- paste0("V", 1:ncol(res))

    goubanjia <- lapply(1:nrow(res), function(i){
        tmp <- res[i, V1] %>% strsplit(., ':') %>% unlist()
        data.table(ip = tmp[1], port = tmp[2])
    }) %>% rbindlist()

    return(goubanjia)
}
## =============================================================================

# fetch_kuaidaili(10)
# fetch_wuyou()
# fetch_jiangxianli(10)
# fetch_crossincode()
# fetch_yundaili(10)
# fetch_66ip()
# fetch_feiyidaili()
# fetch_goubanjia()

# ipTables <- list(
#                  fetch_kuaidaili(10),
#                  fetch_wuyou(),
#                  fetch_jiangxianli(10),
#                  fetch_crossincode(),
#                  fetch_yundaili(10),
#                  fetch_66ip(),
#                  fetch_feiyidaili(),
#                  fetch_goubanjia()
#                  ) %>% rbindlist()


## =============================================================================
h_sina <- c(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,image/apng,*/*;q=0.8",
    "Accept-Encoding" = "gzip, deflate",
    "Accept-Language" = "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Cache-Control" = "max-age=0",
    "Connection" = "keep-alive",
    "DNT" = "1",
    "Host" = "vip.stock.finance.sina.com.cn",
    "If-Modified-Since" = "Tue, 09 Oct 2018 09:11:55 GMT",
    "Referer" = "http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/000001.phtml",
    "Upgrade-Insecure-Requests" = "1",
    "User-Agent" = "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36"
    )

fetchIP <- function(n = 20, core = 8) {

    ipTables <- list(
                     fetch_kuaidaili(n),
                     # fetch_wuyou(),
                     fetch_jiangxianli(n),
                     fetch_crossincode(),
                     # fetch_yundaili(n),
                     fetch_66ip(),
                     fetch_feiyidaili(),
                     fetch_goubanjia()
                     ) %>% rbindlist()

    print("启动并行模式验证 IP 有效性......")
    print(ipTables)

    cl <- makeCluster(core, type = 'FORK')
    ipAvailable <- parSapply(cl, 1:nrow(ipTables), function(i){
        # print(i)
        tmp <- ipTables[i]
        if (class(try(
            r <- GET('http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/000001.phtml',
                   query = list(year = '2018',
                                jidu = '1'),
                   add_headers(h_sina),
                   use_proxy(tmp[1, ip], tmp[1, as.numeric(port)]),
                   timeout(3))
            ,silent = T)) != "try-error") {
            if (r$status_code == '200') {
                if (!is.null(r$header$server)) {
                    if (r$headers$server == 'Sina') return(i)
                }
            }
            }
        }) %>% unlist()
    stopCluster(cl)
    print("完成并行模式验证 IP 有效性......")

    # ipAvailable <- sapply(1:nrow(ipTables), function(i){
    #     print(i)
    #     tmp <- ipTables[i]
    #     if (class(try(
    #         r <- GET('http://vip.stock.finance.sina.com.cn/corp/go.php/vMS_FuQuanMarketHistory/stockid/000001.phtml',
    #                query = list(year = '2018',
    #                             jidu = '1'),
    #                add_headers(h_sina),
    #                use_proxy(tmp[1, ip], tmp[1, as.numeric(port)]),
    #                timeout(3))
    #         ,silent = T)) != "try-error") {
    #         if (r$status_code == '200') {
    #             if (!is.null(r$header$server)) {
    #                 if (r$headers$server == 'Sina') return(i)
    #             }
    #         }
    #         }
    # }) %>% unlist()

    ipTables <- ipTables[ipAvailable]
    ipTables[, tryNo := 0]

    tmp.ipTables <- fread('/home/fl/ipTables.csv')
    tmp <- list(ipTables, tmp.ipTables) %>% 
        rbindlist() %>% .[!duplicated(.)]
    
    fwrite(tmp, '/home/fl/ipTables.csv')

    return(ipTables)
}

## =============================================================================
