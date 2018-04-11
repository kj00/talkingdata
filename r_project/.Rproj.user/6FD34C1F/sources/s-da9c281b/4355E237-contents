##
pacman::p_load(knitr,
               tidyverse,
               data.table,
               lubridate,
               zoo,
               DescTools,
               lightgbm)


##
train_re_path <-
  "/home/paperspace/data/talkingdata/train_reduced.csv"
test_path  <- "/home/paperspace/data/talkingdata/test.csv"

##
train <- fread(train_re_path,
               colClasses = list(numeric = 1:5),
               drop = "attributed_time")

print("The size of the train is: ")
print(object.size(train), units = "auto")
train$click_time %>% typeof


##
train[, `:=` (
  day = substr(click_time, 2, 2) %>% as.integer(),
  hour = substr(click_time, 4, 5) %>% as.integer(),
  minute = substr(click_time, 7, 8) %>% as.integer()
)]

train[, click_time := NULL]


##
days_vec <- unique(train$day)

for (ii in days_vec) {
  fwrite(train[day == ii],
         file = paste0("/home/paperspace/data/talkingdata/train_sep_", ii, ".csv"))
}

rm(train)


##
most_freq_hours_in_test_data <- c(4, 5, 9, 10, 13, 14)
least_freq_hours_in_test_data <- c(6, 11, 15)


##
for (ii in 6:9) {
  

print(paste("day", ii, "train.csv"))
print("reading csv ...")

train_sep <-
  fread(paste0("/home/paperspace/data/talkingdata/train_sep_", ii, ".csv"))

#
print("add in_test_hh")

system.time({
  train_sep[,
            in_test_hh :=
              ifelse(
                hour %in% most_freq_hours_in_test_data,
                1,
                ifelse(hour %in% least_freq_hours_in_test_data, 3, 2)
              )]
  train_sep[,
    "nip_day_test_hh" := .N,
    by = .(ip, day, in_test_hh)]
  train_sep[, "in_test_hh" := NULL]
})


system.time({
  print("n_ip")
  train_sep[
      , "n_ip" := .N,
      by = .(ip, day, hour)]

  print("n_ip_os")
  train_sep[
      , "n_ip_os" := .N,
      by = .(ip, day, hour, os)
      ]
  
  print("n_ip_app")
  train_sep[
      , "n_ip_app" := .N,
      by = .(ip, day, hour, app)
      ]
  
  print("n_ip_app_os")
  train_sep[
      , "n_ip_app_os" := .N,
      by = .(ip, day, hour, app, os)
      ]
  
  print("n_app")
  train_sep[
      ,"n_app" := .N,
      by = .(app, day, hour)
      ]
                                           
})

print("writing csv ...")
fwrite(train_sep,
       file = paste0("/home/paperspace/data/talkingdata/train_sep_", ii, "_added.csv"))

}






