##
pacman::p_load(knitr,
               tidyverse,
               data.table,
               lubridate,
               zoo,
               DescTools,
               lightgbm)

test_path  <- "/home/paperspace/data/talkingdata/test.csv"
test_sup_path  <- "/home/paperspace/data/talkingdata/test_supplement.csv"


test <- fread(test_path,
              colClasses = list(numeric = 2:6),
              showProgress = FALSE)

test_sp <- fread(test_sup_path,
              colClasses = list(numeric = 2:6),
              showProgress = FALSE,
              drop = "click_id")
test_sp <- test_sp[click_time >= "2017-11-10"]
test_sp <- test_sp[click_time < "2017-11-10 04:00:00" | click_time > "2017-11-10 15:00:00"]
test_sp$click_id <- NA

test <- rbind(test, test_sp)
rm(test_sp)

##
test[, `:=` (
  day = substr(click_time, 9, 10) %>% as.integer(),
  hour = substr(click_time, 12, 13) %>% as.integer(),
  minute = substr(click_time, 15, 16) %>% as.integer()
)]

test[, click_time := NULL]


##
most_freq_hours_in_test_data <- c(4, 5, 9, 10, 13, 14)
least_freq_hours_in_test_data <- c(6, 11, 15)


##
print("add in_test_hh")

system.time({
  test[,
       in_test_hh :=
         ifelse(
           hour %in% most_freq_hours_in_test_data,
           1,
           
           ifelse(hour %in% least_freq_hours_in_test_data, 3, 2)
         )]
  test[,
       "nip_day_test_hh" := .N,
       by = .(ip, day, in_test_hh)]
  test[, "in_test_hh" := NULL]
})


system.time({
  print("n_ip")
  test[, "n_ip" := .N,
       by = .(ip, day, hour)]
  
  print("n_ip_os")
  test[, "n_ip_os" := .N,
       by = .(ip, day, hour, os)]
  
  print("n_ip_app")
  test[, "n_ip_app" := .N,
       by = .(ip, day, hour, app)]
  
  print("n_ip_app_os")
  test[, "n_ip_app_os" := .N,
       by = .(ip, day, hour, app, os)]
  
  print("n_app")
  test[, "n_app" := .N,
       by = .(app, day, hour)]
  
})


test <- test[!is.na(click_id)]

fwrite(test, "/home/paperspace/data/talkingdata/test_added.csv")

