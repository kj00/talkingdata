pacman::p_load(tidyverse,
               data.table)
source("1_preprocess/testing_function/process_function.R")

##
dat <-
  fread("../../../data/talkingdata/train.csv",
        nrows = 100000)

##
dat[, `:=`
    (
      day = substr(click_time, 9, 10) %>% as.integer(),
      hour = substr(click_time, 12, 13) %>% as.integer(),
      minute = substr(click_time, 15, 16) %>% as.integer(),
      second = substr(click_time, 18, 19) %>% as.integer()
    )]

dat[, time := 60 * hour + minute + (1 / 60) * second]


##
ip <- "ip"
ip_os <- c("ip", "os")
ip_device_os <- c('ip', 'device', 'os')
ip_channel <- c("ip", "channel")
ip_day <- c('ip', 'day')
ip_app <- c('ip', 'app')
ip_app_os <- c('ip', 'app', 'os')
ip_day_hour <- c('ip', 'day', 'hour')



##
cal_delta(dat, ip_channel, "time", add_fw = T)

##
count_unique(dat, ip, "channel")
count_unique(dat, ip_device_os, "app")
count_unique(dat, ip_day, "hour")
count_unique(dat, ip, "app")
count_unique(dat, ip_app, "os")
count_unique(dat, ip, "device")
count_unique(dat, "app", "channel")

##
cum_count(dat, ip_os)
cum_count(dat, c(ip_app_os, "device"))


##
count_all(dat, ip_day_hour)
count_all(dat, ip_app)
count_all(dat, ip_app_os)



































