# suspension：股票停复牌公告数据

>  从上交所、深交所下载股票停复牌数据：
>
> - fetch_sse_suspension：上交所：[披露 -> 交易提示 -> 停复牌信息](http://www.sse.com.cn/disclosure/dealinstruc/suspension/)
> - fetch_szse_suspension：深交所：[信息披露 -> 交易备忘](http://www.szse.cn/disclosure/memo/index.html)

数据结构：

- stockID：股票代码
- stockName：股票名称
- suspensionStart：停牌开始时间
- suspensionStop：停牌结束时间
- suspensionTime：停牌时间长短
- suspensionReason：停牌原因