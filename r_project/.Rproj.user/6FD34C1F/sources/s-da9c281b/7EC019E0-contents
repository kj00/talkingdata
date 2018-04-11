pacman::p_load(tidyverse, data.table)

train_path <- "/home/paperspace/data/talkingdata/train.csv"
test_path  <- "/home/paperspace/data/talkingdata/test.csv"




##
train_y <- fread(train_path, select = "is_attributed")
train_ip <- fread(train_path, select = "ip")
test_ip <- fread(test_path, select = "ip") 

train_ip_unique <- unique(train_ip$ip)
test_ip_unique <-  unique(test_ip$ip)

rm(test_ip)

train_test_ip_unique <- 
  train_ip_unique[train_ip_unique %in% test_ip_unique]


##
which_one <- which(train_y == 1)
which_zero <- which(train_y == 0)
which_same_ip <- which(train_ip$ip %in% train_test_ip_unique)

##
rm(train_ip, train_y)

length(which_one)


##
fread_selected <- 
  function(num_row) {
    fread(train_path, skip = num_row, nrows = 1, header = F)
  }


##
library(future)
plan(multisession())
))
system.time({
  future_lapply(which_one[1:100],
         fread_selected)
  })


