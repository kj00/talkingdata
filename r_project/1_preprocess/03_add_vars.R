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
test_sup <- fread("/home/paperspace/data/talkingdata/test_supplement.csv")
#test <- fread("/home/paperspace/data/talkingdata/test.csv")
  
  ## add time vars
test_sup[, `:=`
     (
       day = substr(click_time, 9, 10) %>% as.integer(),
       hour = substr(click_time, 12, 13) %>% as.integer(),
       minute = substr(click_time, 15, 16) %>% as.integer(),
       second = substr(click_time, 18, 19) %>% as.integer()
     )]
  


## match columns
train_sep$click_id <- NA
test_sup$is_attributed <- NA

test_sup <- test_sup[, colnames(train_sep), with = F]

## combine
train <- rbind(train_sep, test_sup)

rm(train_sep, test_sup)
gc(T)

## add time
train[, time := 60 * hour + minute + (1 / 60) * second]

# delete second
train[, second := NULL]

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


