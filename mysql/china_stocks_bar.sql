################################################################################
## 用于建立 china_stocks_bar 的数据表。
## 包括：
## 1. daily
################################################################################

CREATE DATABASE `china_stocks_bar` DEFAULT CHARACTER SET utf8 COLLATE utf8_general_ci;

################################################################################
## china_stocks_bar.daily
################################################################################
CREATE TABLE  china_stocks_bar.daily(
    TradingDay      DATE             NOT NULL,          ## 交易日期
    stockID         CHAR(10)         NOT NULL,          ## 日期属性: 
    stockName       VARCHAR(30),                        ## 股票名称: 
    #------------------------------------------------------
    open            DECIMAL(15,3),                      ## 开盘价
    high            DECIMAL(15,3),                      ## 开盘价
    low             DECIMAL(15,3),                      ## 开盘价
    close           DECIMAL(15,3),                      ## 开盘价
    #-----------------------------------------------------
    bAdj            DECIMAL(15,3),                      ## 后复权因子
    #-----------------------------------------------------
    volume          BIGINT,                             ## 交易量， 股
    turnover        DECIMAL(30,3),                      ## 交易额，元
    #-----------------------------------------------------
    -- fcap            DECIMAL(30,3),                      ## 流通市值
    -- tcap            DECIMAL(30,3),                      ## 总市值
    #-----------------------------------------------------
    status          VARCHAR(50),                        ## 交易状态
    upperLimit      DECIMAL(15,3),                      ## 涨停价
    lowerLimit      DECIMAL(15,3),                      ## 跌停价
    isLimit         CHAR(1),                            ## 涨跌停标识
                                                        ## 1. u: 涨停
                                                        ## 2, l：跌停
    ifST            CHAR(1),                            ## 是否 st:
                                                        ## 1. y: 是st
                                                        ## 2. n：不是st
    #-----------------------------------------------------
    PRIMARY KEY (TradingDay, stockID)                   ## 主键唯一，重复不可输入
    );

##----------- INDEX --------------------------------------------------------- ##
CREATE INDEX index_daily
ON china_stocks_bar.daily
(TradingDay, stockID);  
## -------------------------------------------------------------------------- ## 


################################################################################
## china_stocks_bar.price_limit
################################################################################
CREATE TABLE  china_stocks_bar.price_limit(
    TradingDay      DATE             NOT NULL,          ## 交易日期
    stockID         CHAR(10)         NOT NULL,          ## 日期属性: 
    stockName       VARCHAR(30),                        ## 股票名称: 
    #------------------------------------------------------
    isUL            CHAR(1),                            ## 涨跌停标识
                                                        ## 1. u: 涨停
                                                        ## 2, l：跌停
    #-----------------------------------------------------
    PRIMARY KEY (TradingDay, stockID)                   ## 主键唯一，重复不可输入
    );

##----------- INDEX --------------------------------------------------------- ##
CREATE INDEX index_price_limit
ON china_stocks_bar.price_limit
(TradingDay, stockID);  
## -------------------------------------------------------------------------- ## 

