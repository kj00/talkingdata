##
pacman::p_load(knitr,
               tidyverse,
               data.table,
               lubridate,
               zoo,
               DescTools,
               lightgbm)


##
train_re_path <- "/home/paperspace/data/talkingdata/train_reduced.csv"
test_path  <- "/home/paperspace/data/talkingdata/test.csv"

##
train <- fread(train_re_path,
               colClasses = list(numeric = 1:5),
               drop = "attributed_time")

print("The size of the train is: ")
print(object.size(train), units = "auto")


##
train[, `:=` (
  day = substr(click_time, 2, 2) %>% as.integer(),
  hour = substr(click_time, 4, 5) %>% as.integer(),
  minute = substr(click_time, 7, 8) %>% as.integer(),
  secound = substr(click_time, 10, 11) %>% as.integer()
)]

train[, click_time := NULL]


##
days_vec <- unique(train$day)

for (ii in days_vec) {
  fwrite(train[day == ii],
         file = paste0("/home/paperspace/data/talkingdata/train_sep_", ii, ".csv"))
}

rm(train)
