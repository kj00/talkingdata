pacman::p_load(tidyverse,
               data.table)


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


count_unique(dat, "ip", "app")
count_all(dat, "ip")
cum_count(dat, "ip")
cal_delta(dat, "ip", time_var ="time", T)
cal_var(dat, "ip", "hour")


