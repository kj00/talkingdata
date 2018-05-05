## read package
pacman::p_load(tidyverse,
               data.table)

## read functions
source("1_preprocess/testing_function/process_function.R")

## read train
nrows=184903891#-1
nchunk=25000000
val_size=2500000
frm=nrows-65000000

# if debug:
#   frm=0
# nchunk=100000
# val_size=10000

to=frm+nchunk
nchunk

train_col <-   fread("/home/paperspace/data/talkingdata/train.csv",
                     nrow=1,
                     drop = "attributed_time") %>%colnames()

train <-
  fread("/home/paperspace/data/talkingdata/train.csv",
        drop = 7, skip = frm, nrow = nchunk, header = F)


colnames(train) <- train_col

## read test
test <- fread("/home/paperspace/data/talkingdata/test_supplement.csv")
#test <- fread("/home/paperspace/data/talkingdata/test.csv")
  

## match columns
train[, click_id := NA]
test[, is_attributed := NA]


## combine
train <- rbind(train, test)

rm(test)
gc(T)


## add time vars
train[,  `:=` (
  day = substr(click_time, 9, 10) %>% as.integer(),
  hour = substr(click_time, 12, 13) %>% as.integer(),
  minute = substr(click_time, 15, 16) %>% as.integer(),
  second = substr(click_time, 18, 19) %>% as.integer()
)]

train[, click_time := NULL]


## add time
train[, time := 60 * (day * 24 + hour) + minute + (1 / 60) * second]

# delete day and second
train[, `:=` (
  second = NULL)]

gc(T)

## group vars
ip <- "ip"
ip_os <- c("ip", "os")
ip_device_os <- c('ip', 'device', 'os')
ip_channel <- c("ip", "channel")
ip_day <- c('ip', 'day')
ip_app <- c('ip', 'app')
ip_app_os <- c('ip', 'app', 'os')
ip_day_hour <- c('ip', 'day', 'hour')


##
#train<- train[1:1000000]
cal_delta_both(train, c("ip", "app", "device", "os", "channel"), time_var = "time", add_prev = F)
cal_delta_both(train, ip_device_os, time_var = "time", add_prev = F)
cal_delta_both(train, c(ip_device_os, "app"), time_var = "time", add_prev = F)

cal_delta_only_prev(train, ip_channel, "time")
cal_delta_only_prev(train, ip_os, "time")


train[, time := NULL]

##
gc(T)
count_unique(train, ip, "channel")
count_unique(train, ip_device_os, "app")
count_unique(train, ip_day, "hour")
count_unique(train, ip, "app")
count_unique(train, ip_app, "os")
count_unique(train, ip, "device")
count_unique(train, "app", "channel")

##
gc(T)
cum_count(train, ip_os)
cum_count(train, c(ip_app_os, "device"))


##
gc(T)
count_all(train, ip_day_hour)
count_all(train, ip_app)
count_all(train, ip_app_os)

##
cal_var(train, ip_app_os, "hour")


## save
colnames(train)

# save train
fwrite(train[!is.na(is_attributed), -"click_id"],
       "tmp/train_added.csv",
       nThread = 4)


# reduce data size
train <- train[is.na(is_attributed)]
  

# save test
click_id_master <- fread("/home/paperspace/data/talkingdata/test_click_id_relation.csv")

train <- merge(click_id_master, train,
               by.x = "click_id.testsup",
               by.y = "click_id",
               all.x=T)

train[, click_id.testsup := NULL]
train[, is_attributed := NULL]

train <- train[order(click_id.test)]

colnames(train)[1] <- "click_id"


fwrite(train,
       "tmp/test_added.csv",
       nThread = 4)
