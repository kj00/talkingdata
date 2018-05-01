test <- fread(test_path,
              colClasses = list(numeric = 2:6),
              showProgress = FALSE,select =c("click_time", "click_id"))

test_sp <- fread(test_sup_path,
                 colClasses = list(numeric = 2:6),
                 showProgress = FALSE,
                 select = "click_time")



test[, `:=`
     (
       day = substr(click_time, 9, 10) %>% as.integer(),
       hour = substr(click_time, 12, 13) %>% as.integer(),
       minute = substr(click_time, 15, 16) %>% as.integer()
#       second = substr(click_time, 18, 19) %>% as.integer()
     )]


test_sp[, `:=`
     ( click_id = NA,
       day = substr(click_time, 9, 10) %>% as.integer(),
       hour = substr(click_time, 12, 13) %>% as.integer(),
       minute = substr(click_time, 15, 16) %>% as.integer()
       #       second = substr(click_time, 18, 19) %>% as.integer()
     )]

test <- rbind(test, test_sp)
rm(test_sp)


test[, .(uniqueN(minute), min(minute), max(minute),
         count=.N),
     by = .(click_id=is.na(click_id), day,hour)]

test[!is.na(click_id), .(minute = unique(minute)),
     by = .(day,hour)]




