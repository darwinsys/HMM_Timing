source("gmmhmm.R")


## 测试中证500
########################################################################
## 0. 读取指数数据
load_data <- function()
{
  data <- read.csv("data/NAV_5ETFs.csv")
  zz500 <- as.xts(data[, 2], order.by=strptime(data[, 1], format="%Y-%m-%d", tz=""))
  zz500 <- na.omit(cbind(zz500, Return.calculate(zz500)))
  benchmark <- as.xts(data[, 2:6], order.by=strptime(data[,1], format="%Y-%m-%d", tz=""))
  benchmark_ret <- na.omit(Return.calculate(benchmark, method = "discrete"))
  colnames(zz500) <- c('zz500', 'zz500.ret')
  
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
  ema2 <- TTR::EMA(benchmark[,1], 2);
  data_oot <- cbind.xts(benchmark_ret[, c(1, 2, 5)],  Return.calculate(ema2), lag(benchmark_ret[,1], 1), lag(benchmark_ret[, 1], 5));
  data_oot <- na.omit(data_oot)
  ret <- gmmhmm(dataset = data_oot, ret_target = benchmark_ret[, 1], n_start = 1000, n_state = 5)
}


### running tests
data <- load_data();
benchmark <- data$benchmark;
benchmark_ret <- data$benchmark_ret;Date

rets <- test_oot2();

