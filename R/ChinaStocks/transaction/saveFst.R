## =============================================================================
## saveFst.R
## 读取所有的 Tick 数据, 并保存为 fst 文件
## =============================================================================

## =============================================================================
setwd('/home/fl/myData/')
suppressMessages({
  source('./R/Rconfig/myInit.R')
  source('./R/Rconfig/stocks.R')
})
library(fst)
## =============================================================================


## =============================================================================
dataPath <- '/home/fl/myData/data/ChinaStocks/Transaction/FromSina'
savePath <- '/data/ChinaStocks/TickData/FromSina'
allYears <- dir(dataPath) %>% .[. >= '2007']
for (y in allYears) {
    p <- paste(savePath, y, sep = '/')
    if (!dir.exists(p)) dir.create(p)
}
## =============================================================================

for (yr in allYears) {

    allDays <- paste(dataPath, yr, sep = '/') %>% dir()

    for (tday in allDays) {

        allFiles <- tday %>% 
            paste(dataPath, yr, ., sep = '/') %>% 
            list.files(., pattern = '\\.csv$', full.names = TRUE)

        destfile <- sprintf("%s/%s/%s.fst", savePath, yr, tday)
        
        # if (file.exists(destfile)) {
        #     print(sprintf("## %s file exited :==> @%s", tday, Sys.time()))
        #     next
        # }

        if (length(allFiles) == 0) {
            res <- data.table(证券代码 = NA)
        } else {
            res <- lapply(allFiles, function(f){
                # print(f)
                res <- readSinaTick(f) %>% 
                    .[, ":="(证券代码 = gsub('.*([0-9]{6})\\.csv$', '\\1', f)
                     )]
            }) %>% rbindlist(., fill = TRUE)
            setcolorder(res, c('证券代码',
                               colnames(res)[1:(ncol(res)-1)])
                        )
        } 

        print(sprintf("## %s file saved  :==> @%s", tday, Sys.time()))
        fst::write.fst(res, destfile, 100)
    }

}

## =============================================================================
# no.cores <- max(round(detectCores()/4), 12)
# cl <- makeCluster(length(allYears), type="FORK")
# parLapply(cl, allYears, function(yr){
#     allDays <- paste(dataPath, yr, sep = '/') %>% dir()

#     for (tday in allDays) {
#         print(sprintf("## %s :==> @%s", tday, Sys.time()))

#         allFiles <- tday %>% 
#             paste(dataPath, yr, ., sep = '/') %>% 
#             list.files(., pattern = '\\.csv$', full.names = TRUE)

#         if (length(allFiles) == 0) {
#             res <- data.table()
#         } else {
#             res <- lapply(allFiles, function(f){
#                 res <- readSinaTick(f) %>% 
#                     .[, ":="(证券代码 = gsub('.*([0-9]{6})\\.csv$', '\\1', f)
#                      )]
#             }) %>% rbindlist()
#             setcolorder(res, c('证券代码',
#                                colnames(res)[1:(ncol(res)-1)])
#                         )
#         }

#         fst::write.fst(res,
#                        sprintf("%s/%s/%s.fst", savePath, yr, tday),
#                        100)
#     }

# })
