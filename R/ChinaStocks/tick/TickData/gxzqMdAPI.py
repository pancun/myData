#!/usr/bin/env python
# -*- coding:utf-8 -*- 

## =============================================================================
import sys
import os
reload(sys)
sys.setdefaultencoding('utf8')

import requests
from bs4 import BeautifulSoup
import pandas as pd
pd.options.display.float_format = '{:,.2f}'.format
from pprint import pprint
from datetime import datetime
from time import sleep
import json
import MySQLdb
import csv

import ast
import re
import multiprocessing

pd.set_option('display.width', 200)
## =============================================================================

BrokerID = u'国信证券'
currTradingDay = datetime.now().strftime("%Y%m%d")
DATA_PATH = '/data/ChinaStocks/TickData/FromGXZQ/%s' % currTradingDay

################################################################################
## william
## 从 MySQL 数据库查询数据
################################################################################
#-------------------------------------------------------------------------------
def fetchMySQL(db, query):
    """ 从 MySQL 中读取数据 """
    try:
        conn = MySQLdb.connect(
            db = db, 
            host = '192.168.1.166', 
            port = 3306, 
            user = 'fl', 
            passwd = 'abc@123', 
            use_unicode = True, 
            charset = "utf8")
        cursor = conn.cursor()
        mysqlData = pd.read_sql(query, conn)
        return mysqlData
    except (MySQLdb.Error, MySQLdb.Warning, TypeError) as e:
        print e
        return None
    finally:
        conn.close()
#-------------------------------------------------------------------------------


## =============================================================================
calendar  = fetchMySQL(db = 'dev',
                       query = 'select * from ChinaFuturesCalendar')
## -----------------------------------------------------------------------------
if datetime.now().date() not in calendar.days.values:
    sys.exit('Not TradingDay!!!')
## -----------------------------------------------------------------------------

if not os.path.exists(DATA_PATH):
    os.makedirs(DATA_PATH)
## =============================================================================

headersGuoXin = {
    "Accept" : "application/json, text/javascript, */*; q=0.01",
    "Accept-Encoding" : "gzip, deflate, br",
    "Accept-Language" : "zh-CN,zh;q=0.9,en-US;q=0.8,en;q=0.7,zh-TW;q=0.6",
    "Connection" : "keep-alive",
    "DNT" : "1",
    "Gapp-Viewnum" : "1",
    "Gapp-XNo" : "bb5036c2-7b5d-4461-bf8d-1e5ff609cd75",
    "Host" : "trade2.guosen.com.cn",
    "Referer" : "https://trade2.guosen.com.cn/eagle-frontap/default?t=0&m=0&acc=true",
    "User-Agent" : "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/68.0.3440.84 Safari/537.36",
    "x-action" : "Grid%E5%88%97%E8%A1%A8%3A%E6%9B%B4%E5%A4%9A(%23component_50%20DIV.all-trade-btn)%E8%A2%AB%E5%8D%95%E5%87%BB%E4%BA%86",
    "X-Requested-With" : "XMLHttpRequest",
    "x-screen-size" : "1920x1080",
    "x-uid" : "Guest",
    "x-view-name" : "%E8%87%AA%E9%80%89%E8%82%A1"
            }

## 市场： template.min.js?
## 
## symbols/ / stockID / marketID / AB
## 
## marketID :   1 > SH
##              2 > SZ
## 
## symbolType:  
## 
## dataType:  复权类型

## =============================================================================
url = "https://trade2.guosen.com.cn/eagle-frontap/resource/codelist"
r = requests.get(url, headers = headersGuoXin)

## -----------------------------------------------
data = r.content.replace("gxCodeList={data:[", "")
data = data[:-3]
for k in ['t','s','m','n','p','d','h','r','l']:
    data = data.replace(k + ':', '"' + k + '":')
data = data.replace('t"h":', '"th":')
data = data.replace('null', '"null"')
## -----------------------------------------------

## ------------------------------------------------------
temp = ast.literal_eval(data)
codeDict = {x['s'] + '-' + str(x['m']): x for x in temp}
## ------------------------------------------------------
## =============================================================================

faildeList = []

def fetchTrade(code):
    """获取分笔交易数据"""
    code = str(code)

    ## -------------------------------------------------------------------------
    destFile = DATA_PATH + '/%s.csv' %code
    if os.path.isfile(destFile):
        print u'%s 数据文件已下载' %code
        return
    ## -------------------------------------------------------------------------

    ## ----------------------------------------
    mk = '1' if code[0] == '6' else '2'
    k = code+'-'+mk
    info = codeDict.get(k)
    if not info:
        print u'%s 数据文件不存在' %code
        return
    ## ----------------------------------------

    ## -------------------------------------------------------------------------
    url = "https://trade2.guosen.com.cn/eagle-frontap/resource/symbols/{symbolType}/{symbol}/{marketID}/1/trades".format(symbolType = info['t'],
        symbol = code,
        marketID = mk)
    payload = {"fromNo":1,
               "toNo":10000
               }
    ## -------------------------------------------------------------------------
    try:
        r = requests.get(url, headers = headersGuoXin,
                         params = payload, timeout = 10)
    except:
        print u"%s 连接服务器失败" %code
        faildeList.append(code)
        return
    ## -------------------------------------------------------------------------
    data = r.json()['symbolTrades']
    ## -------------------------------------------------------------------------

    ## -------------------------------------------------------------------------
    dfData = []
    dfHeader = ['time','ask','bid','presentPrice','bs','hand','volume',
                'largeOrder','pricePrecision','stroke']
    for d in data:
        tmp = [d[k] for k in dfHeader]
        dfData.append(tmp)
    df = pd.DataFrame(dfData, columns = dfHeader)
    df.sort_values(by='time', ascending = True, inplace = True)
    df.reset_index(drop = True, inplace = True)
    df['time'] = df['time'].apply(lambda x: datetime.fromtimestamp(x/1000).strftime("%Y-%m-%d %H:%M:%S"))
    df['stroke'] = df['stroke'].apply(lambda x: str(x))
    ## -------------------------------------------------------------------------

    ## ----------------------------
    with open(destFile, 'wb') as f:
        df.to_csv(f, index = False)
    ## ----------------------------
    
    return df

# data = fetchTrade('600516')
# data = fetchTrade('000004')
# data = fetchTrade('002001')
# data = fetchTrade('300005')


symbolList = fetchMySQL(db = 'china_stocks_info', 
                        query = "select * from stocks_list")

## =============================================================================
t1 = datetime.now()
for i in range(5):
    pool = multiprocessing.Pool(processes = 8)
    pool.map(fetchTrade, symbolList.stockID.values)

if faildeList:
    for i in range(5):
        pool = multiprocessing.Pool(processes = 4)
        pool.map(fetchTrade, faildeList)
t2 = datetime.now()
print (t2 -t1).total_seconds()
## =============================================================================

