## read package
pacman::p_load(tidyverse,
               data.table)

## read functions
source("1_preprocess/testing_function/process_function.R")

## read train @day9
ii <- 9

train_sep <-
  fread(paste0("/home/paperspace/data/talkingdata/train_sep_", ii, ".csv"))

colnames(train_sep) <- 
  stringr::str_replace(colnames(train_sep), "secound", "second")


## read test
test <- fread("/home/paperspace/data/talkingdata/test_combined.csv")


## combine
train_sep$click_id <- NA
test$is_attributed <- NA

test <- test[, colnames(train_sep), with = F]

train <- rbind(train_sep, test)

rm(train_sep, test)

## add time
train[, time := 60 * hour + minute + (1 / 60) * second]

# delete second
train[, second := NULL]

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
gc(T)
cal_delta(train, ip_channel, time_var = "time", add_fw = T)
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

## save
colnames(train)
train <- train[!is.na(is_attributed) | !is.na(click_id)]

# save train
fwrite(train[!is.na(is_attributed), -"click_id"],
       "tmp/train_added.csv",
       nThread = 4)
# save test
fwrite(train[is.na(is_attributed)],
       "tmp/test_added.csv",
       nThread = 4)
