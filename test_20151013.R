source("gmmhmm.R")


## 测试中证500
########################################################################
## 0. 读取指数数据
load_data <- function()
{
  data <- read.csv("data/NAV_5ETFs_updated.csv")
  benchmark <- na.omit(as.xts(data[, 2:6], order.by=strptime(data[,1], format="%Y/%m/%d", tz="")))
  benchmark_ret <- na.omit(Return.calculate(benchmark, method = "discrete"))
  
  d <- list();
  d[[1]] <- benchmark;
  d[[2]] <- benchmark_ret;
  return (d);
}
load_data()



## out-of-sample test for 中证500
#######################################
source("gmmhmm.R")
zz500 <- benchmark[, 1]
hs300 <- benchmark[, 2]
spx <- benchmark[, 3]
gold <- benchmark[, 4]
hengsheng <- benchmark[, 5]

ret_zz500_d <-  TTR::ROC(zz500, n=1, "discrete")
ret_zz500_2d <- TTR::ROC(zz500, n=2, "discrete")
ret_zz500_2d <- na.omit(zz500 / lag(zz500, 2) - 1)
ret_zz500_5d <- na.omit(zz500 / lag(zz500, 5) - 1)
ret_zz500_20d <- na.omit(zz500 / lag(zz500, 10) - 1)

ret_hs300_d <- na.omit(hs300 / lag(hs300, 1) - 1)
ret_hs300_2d <- na.omit(hs300 / lag(hs300, 2) - 1)
ret_hs300_5d <- na.omit(hs300 / lag(hs300, 5) - 1)
ret_hs300_20d <- na.omit(hs300 / lag(hs300, 20) - 1)


zz500_w <- zz500[endpoints(zz500, on = "weeks")]
hs500_w <- hs300[endpoints(hs300, on = "weeks")]

ret_zz500_w <- na.omit(zz500_w / lag(zz500_w, 1) - 1)
ret_zz500_w_5d <- na.omit(zz500_w / lag(zz500_w, 5) - 1)

ret_zz500_spread <- ret_zz500_d - lag(ret_zz500_d, 1);


## Testing out-of-sample for 5ETF portfolio returns
########################################################
source("gmmhmm.R")
data <- read.csv("data/portfolio_5etfs_ret_0.0008_weekly.csv")
benchmark <- na.omit(as.xts(data[, 2:7], order.by=strptime(data[,1], format="%Y-%m-%d", tz="")))
portfolio <- benchmark[, 1]

data_1 <- cbind(portfolio, lag(portfolio, 1), lag(portfolio, 5))
data_1 <- na.omit(data_1)
ret <- gmmhmm1(dataset=data_1, ret_target=data_1[, 1], n_start=1000, n_state = 5)




#######################################################
test_oot3 <- function() {
  source("gmmhmm.R")
  data_oot3 <- cbind.xts( TTR::ROC(zz500, n = 1),
                          TTR::ROC(zz500, n = 2), 
                          TTR::ROC(hs300, n = 2)
    )
  data_oot3 <- na.omit(data_oot3[is.infinite(data_oot3[,1]) == FALSE]);
  ret <- gmmhmm2(dataset=data_oot3, ret_target=ret_zz500_d[index(data_oot3)], n_start=1000, n_state=5)
}

test_oot4 <- function() {
  source("gmmhmm.R")
  data_oot4 <- cbind.xts( TTR::ROC(zz500, n = 1),
                          TTR::ROC(zz500, n = 2) / 2, 
                          TTR::ROC(hs300, n = 2) / 2
                          
  )
  data_oot4 <- na.omit(data_oot4);
  ret <- gmmhmm2(dataset=data_oot4, ret_target=ret_zz500_d[index(data_oot4)], n_start=1500, n_state=5)
}

test_oot5 <- function() {
  source("gmmhmm.R")
  data_oot5 <- cbind.xts( TTR::ROC(zz500, n = 1),
                          TTR::ROC(zz500, n = 2) / 2, 
                          TTR::ROC(hs300, n = 2) / 2,
                          TTR::ROC(zz500, n = 5) / 5, 
                          TTR::ROC(zz500, n = 20)
                          
                          
  )
  data_oot5 <- na.omit(data_oot5);
  ret <- gmmhmm2(dataset=data_oot5, ret_target=ret_zz500_d[index(data_oot5)], n_start=1500, n_state=5)
}


test_oot3 <- function() {
  source("gmmhmm.R")
  data_oot3 <- cbind.xts( TTR::ROC(zz500, n = 1),
                          TTR::ROC(zz500, n = 2) / 2, 
                          TTR::ROC(hs300, n = 2) / 2
                          
  )
  data_oot3 <- na.omit(data_oot3);
  ret <- gmmhmm2(dataset=data_oot3, ret_target=ret_zz500_d[index(data_oot3)], n_start=1000, n_state=5)
}


test_oot31 <- function() {
  data_oot3 <- cbind.xts( ret_zz500_d,
                          ret_zz500_5d, 
                          ret_zz500_20d
                          
  )
  data_oot3 <- na.omit(data_oot3);
  ret <- gmmhmm2(dataset=data_oot3, ret_target=ret_zz500_d[index(data_oot3)], n_start=1000, n_state=0)
}


















test_oot4 <- function() {
  data_oot4 <- cbind.xts( ret_zz500_d,
                          ret_hs300_d, 
                          ret_zz500_5d, )
  data_oot4 <- na.omit(data_oot4);
  ret <- gmmhmm(dataset = data_oot4, ret_target = ret_zz500_d[index(data_oot4)], n_start=1000, n_state = 5)
  
}


test_oot1 <- function() {
  data_oot1 <- cbind.xts(zz500,
                         zz500 - TTR::EMA(zz500, 2),
                         zz500 - TTR::EMA(zz500, 5),
                         TTR::RSI(zz500))
                         #ret_zz500_5d - lag(ret_zz500_5d, 1),
                         #ret_zz500_5d - lag(ret_zz500_5d, 5))
  data_oot1 <- na.omit(data_oot1)
  ret <- gmmhmm1(dataset = data_oot1, ret_target = ret_zz500_d[index(data_oot1)], n_start = 500, n_state = 5)
}

test_oot2 <- function() {
  data_oot2 <- cbind.xts(ret_zz500_w,  lag(ret_zz500_w, 1),
                         ret_zz500_w_5d, lag(ret_zz500_w_5d, 1))
  data_oot2 <- na.omit(data_oot2)
  ret <- gmmhmm1(dataset = data_oot2, ret_target = data_oot2[, 1], n_start = 300, n_state = 3)
 }

test_oot2 <- function() {
  data_oot2 <- cbind.xts(zz500, TTR::EMA(zz500, 2), TTR::EMA(zz500, 10))
  data_oot2 <- na.omit(data_oot2)
  ret <- gmmhmm(dataset = data_oot2, ret_target = ret_zz500_d[index(data_oot2)], 
                n_start = 500, n_state = 5)
  
}

test_oot1 <- function() {
  
  data_oot <- cbind.xts(benchmark_ret[, c(1, 2, 5)], lag(benchmark_ret[,1], 1), lag(benchmark_ret[, 1], 5));
  data_oot <- na.omit(data_oot)
  gmmhmm(dataset = data_oot, ret_target = benchmark_ret[, 1], n_start = 500, n_state = 3)
}

test_oot2 <- function() {
  ret_zz500 <- benchmark_ret[, 1]
  ret_hs300 <- benchmark_ret[, 2]
  ret_hengsheng <- benchmark_ret[, 5]
  
  ema2 <- TTR::EMA(benchmark[,1], 2);
  ema5 <- TTR::EMA(benchmark[,1], 5);
  data_oot <- cbind.xts(ret_zz500, ret_zz500 - ret_hs300, ret_zz500 - ret_hengsheng,
                        Return.calculate(ema2),  Return.calculate(ema5),
                        #           Return.calculate(TTR::EMA(benchmark[, 2]), 2),
                        lag(ret_zz500, 1), lag(ret_zz500, 5));
  data_oot <- na.omit(data_oot)
  ret <- gmmhmm(dataset = data_oot, ret_target = benchmark_ret[, 1], n_start = 1000, n_state = 5)
  write.csv(as.data.frame(ret), 'test_results/oot2.csv')
}
