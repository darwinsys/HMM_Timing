source("gmmhmm.R")


## 测试中证500
########################################################################
## 0. 读取指数数据
load_data <- function()
{
  data <- read.csv("data/NAV_5ETFs.csv")
  benchmark <- as.xts(data[, 2:6], order.by=strptime(data[,1], format="%Y-%m-%d", tz=""))
  benchmark_ret <- na.omit(Return.calculate(benchmark, method = "discrete"))
  
  d <- list();
  d[[1]] <- benchmark;
  d[[2]] <- benchmark_ret;
  return (d);
}


## out-of-sample test for 中证500
#######################################
source("gmmhmm.R")
test_oot1 <- function() {
  data_oot <- cbind.xts(benchmark_ret[, c(1, 2, 5)], lag(benchmark_ret[,1], 1), lag(benchmark_ret[, 1], 5));
  data_oot <- na.omit(data_oot)
  gmmhmm(dataset = data_oot, ret_target = benchmark_ret[, 1], n_start = 1000, n_state = 5)
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

test_oot3 <- function() {
  ret_zz500 <- benchmark_ret[, 1]
  ret_hs300 <- benchmark_ret[, 2]
  ret_hengsheng <- benchmark_ret[, 5]
  
  ret_zz500_ema2 <- Return.calculate(TTR::EMA(benchmark[,1], 2))
  ret_zz500_ema5 <- Return.calculate(TTR::EMA(benchmark[,1], 5))
  ret_hs300_ema2 <- Return.calculate(TTR::EMA(benchmark[,2], 2))
  ret_hs300_ema5 <- Return.calculate(TTR::EMA(benchmark[,2], 5))
  
  
  data_oot3 <- cbind.xts(ret_zz500, ret_zz500 - ret_hs300, ret_zz500 - ret_hengsheng,
                        ret_zz500_ema2, ret_zz500_ema5,
                        ret_zz500_ema2 - ret_hs300_ema2,
                        #           Return.calculate(TTR::EMA(benchmark[, 2]), 2),
                        lag(ret_zz500, 1), lag(ret_zz500, 5));  
  data_oot3 <- na.omit(data_oot3)
  data_oot3 <- data_oot3[500:1596, ]
  ret_oot3 <- gmmhmm(dataset = data_oot3, ret_target = data_oot3[, 1], n_start = 1000, n_state = 5)
  rbind(table.AnnualizedReturns(ret_oot3), SharpeRatio(ret_oot3), maxDrawdown(ret_oot3))
  write.csv(as.data.frame(ret), 'test_results/oot3.csv')
}

## 使用最新的数据（截止到2015年10月的
test_oot4 <- function() {
  data <- read.csv("data/Index_3.csv")
  benchmark <- as.xts(data[, 2:4], order.by=strptime(data[,1], format="%Y/%m/%d", tz=""))
  benchmark_w <- benchmark[endpoints(benchmark, on = "weeks")]
  benchmark_ret <- na.omit(Return.calculate(benchmark, method = "discrete"))
  benchmark_ret_w <- na.omit(Return.calculate(benchmark_w, method="discrete"))
  
  ret_zz500 <- benchmark_ret[, 1]
  ret_hs300 <- benchmark_ret[, 2]
  ret_hengsheng <- benchmark_ret[, 3]
  
  ret_zz500_ema2 <- Return.calculate(TTR::EMA(benchmark[,1], 2))
  ret_zz500_ema5 <- Return.calculate(TTR::EMA(benchmark[,1], 5))
  ret_hs300_ema2 <- Return.calculate(TTR::EMA(benchmark[,2], 2))
  ret_hs300_ema5 <- Return.calculate(TTR::EMA(benchmark[,2], 5))
  
  
  data_oot4 <- cbind.xts(ret_zz500, ret_zz500 - ret_hs300, #ret_zz500 - ret_hengsheng,
                         ret_zz500_ema2, ret_zz500_ema5,
                         #           Return.calculate(TTR::EMA(benchmark[, 2]), 2),
                         lag(ret_zz500, 1), lag(ret_zz500, 5));  
  data_oot4 <- na.omit(data_oot4)
  ret_oot4 <- gmmhmm(dataset = data_oot4, ret_target = data_oot4[, 1], n_start = 1000, n_state = 5)
  rbind(table.AnnualizedReturns(ret_oot4), SharpeRatio(ret_oot4), maxDrawdown(ret_oot4))
  write.csv(as.data.frame(ret), 'test_results/oot4.csv')
  
  
}


### running tests
data <- load_data();
benchmark <- data$benchmark;
benchmark_ret <- data$benchmark_ret;Date

rets <- test_oot2();

