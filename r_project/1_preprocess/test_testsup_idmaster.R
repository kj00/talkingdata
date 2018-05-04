library(data.table)

start_time <- Sys.time()

test <- fread("/home/paperspace/data/talkingdata/test.csv", colClasses = list(numeric=2:6))
testsup <- fread("/home/paperspace/data/talkingdata/test_supplement.csv", colClasses = list(numeric=2:6))

# Add dup_order column to specify the order of duplicate rows
test[, dup_order := 1:.N, by = .(ip, app, device, os, channel, click_time)]
testsup[, dup_order := 1:.N, by = .(ip, app, device, os, channel, click_time)]

# Joining data the data.table way
setkey(test, ip, app, device, os, channel, click_time, dup_order)
setkey(testsup, ip, app, device, os, channel, click_time, dup_order)

result <- testsup[test][, c("ip", "app", "device", "os", "channel", "click_time", "dup_order") := NULL]
setnames(result, c("click_id.testsup", "click_id.test"))
setcolorder(result, c("click_id.test", "click_id.testsup"))
result <- result[order(click_id.test)]

dim(test)
dim(result)
head(test)
head(result)

# save the relation for later use
fwrite(result, "/home/paperspace/data/talkingdata/test_click_id_relation.csv")
