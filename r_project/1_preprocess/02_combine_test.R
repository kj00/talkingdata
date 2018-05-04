##
pacman::p_load(tidyverse,
               data.table)


test_path  <- "/home/paperspace/data/talkingdata/test.csv"
test_sup_path  <- "/home/paperspace/data/talkingdata/test_supplement.csv"


test <- fread(test_path,
              colClasses = list(numeric = 2:6),
              showProgress = FALSE)

testtest_sp <- fread(test_sup_path,
                 colClasses = list(numeric = 2:6),
                 showProgress = FALSE)
#                 drop = "click_id")

test_sp <- test_sp[click_time >= "2017-11-10"]

test_sp$click_id <- NA

test <- rbind(test, test_sp)
rm(test_sp)

##
test[, `:=`
     (
       day = substr(click_time, 9, 10) %>% as.integer(),
       hour = substr(click_time, 12, 13) %>% as.integer(),
       minute = substr(click_time, 15, 16) %>% as.integer(),
       second = substr(click_time, 18, 19) %>% as.integer()
     )]


##
test_sup_dif <- 
  fsetdiff(
    # test_sup
    test[is.na(click_id), .(minute = unique(minute)),
         by = .(hour)],
    # test
    test[!is.na(click_id), .(minute = unique(minute)),
         by = .(hour)],
    all = TRUE) 


test_cl <- colnames(test)

test <- 
merge(
  test_sup_dif,
  test[is.na(click_id)],
  by = c("hour", "minute")) %>% 
  .[, test_cl, with = F] %>% 
  rbind(test[!is.na(click_id)])

# check
test[, .(uniqueN(minute), min(minute), max(minute),
         count=.N),
     by = .(click_id=is.na(click_id), day,hour)]


##
fwrite(test, "/home/paperspace/data/talkingdata/test_combined.csv",
       nThread = 8)





