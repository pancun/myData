# ChinaStocks

# 交易所数据

## 上海交易所

-   在交易所可以查到当天所有已经上市的股票信息：[产品 -> 股票 -> 股票列表](http://www.sse.com.cn/assortment/stock/list/share/)
-   每只股票的详细信息：[首页 -> 产品 -> 股票 -> 股票列表 -> 股票概况 -> 公司概况](http://www.sse.com.cn/assortment/stock/list/info/company/index.shtml?COMPANY_CODE=600000)
-   可以从股票的成交概况来判断当天是不是有交易：[首页 -> 产品 -> 股票 -> 股票列表 -> 股票概况 -> 成交概况](http://www.sse.com.cn/assortment/stock/list/info/turnover/index.shtml?COMPANY_CODE=600004)
-   查询股票分红、送股数据：[数据 -> 股票数据 -> 分红/送股](http://www.sse.com.cn/market/stockdata/dividends/dividend/)
-   可以查到已退市的股票信息：[服务 -> 已退市公司信息](http://www.sse.com.cn/services/information/delisting/)
-   查询从 2018年4月 以后的所有股票停牌与复牌日期安排：[披露 -> 交易提示 -> 停复牌信息](http://www.sse.com.cn/disclosure/dealinstruc/suspension/)
    -   上交所的数据是从 `2008-04-01` 开始公布的

## 深圳交易所

-   已上市股票列表：
    -   [市场数据 -> 上市公司 -> 上市公司列表](http://www.szse.cn/market/companys/company/index.html)
    -   [市场数据 -> 股票 -> 股票列表](http://www.szse.cn/market/stock/list/index.html)
-   退市股票列表：[市场数据 -> 上市公司 -> 暂停/终止上市公司](http://www.szse.cn/market/companys/suspend/index.html)
-   停复牌信息：[信息披露 -> 交易备忘](http://www.szse.cn/disclosure/memo/index.html)
-   历史行情数据：[市场数据 -> 行情](http://www.szse.cn/market/trend/index.html)
-   市场事后统计汇总：[市场数据 -> 电子期刊 -> 统计月报](http://www.szse.cn/market/periodical/month/index.html)



# 新浪[历史]股票成交明细

-   以前可以直接下载整个 tick 文件，不过现在之能一页一页查询数据，然后再拼接成一个文件
-   可以查询到股票历史的所有分红、送股、配股等数据



## Tick Data 处理过程

### 数据明显错误

 - NA
 - 负值
 - 异常大的数值
 - updateTime 不在正常交易时间段：09:20～11:30，13:00～15:00
 - 没有 updateTime
 - 有成交价格，没有成交量
 - 有成交量，没有成交价格
 - 当天停牌的股票，没有成交数据

### 数据冗余

- 整行数据重复
- updateTime 重复(有可能是第二个没有更新)

### 不符合逻辑

- 总成交手数和总成交额是 `cummax`
- deltaVolume、deltaTurnover 等于成交手续、成交额
- price * volume 与 turnover 相差在一手以内
- bid 和 ask 价格位置分别是**递减**、**递增**
- lower <= bidPrice <= price <= askPrice <= upper
- 涨停时：
  - price == bidPrice1 == upper
  - bidVolume >0，askVolume == 0

- 跌停时：
  - price == askPrice1 == lower
  - bidVolume == 0，askVolume > 0

### 数据核对

- tick 数据需要检查断点 （排除停牌后，间断超过 30 分钟的）
- minute 与其他数据源对比（有细微差异）
- daily 与 **新浪**、**163** 对比

