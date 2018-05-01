##
source("process_data.R")

##
for (ii in #6:
     9) {
  
  
  print(paste("day",ii, "train.csv"))
  print("reading csv ...")
  
  train_sep <-
    fread(paste0("/home/paperspace/data/talkingdata/train_sep_", ii, ".csv"))
  
  colnames(train_sep) <- 
    stringr::str_replace(colnames(train_sep), "secound", "second")
  
  
  #process
  print("process data")

  # since test and test_sup is restricted  
  #train_sep <- train_sep[hour %in% 0:15]
  train_sep <- process_data(train_sep, is_train = T)

  
  if(train_sep %>% map_lgl(~ any(is.na(.x))) %>% any()) {
    stop("there is na!")
  }
  
  print(paste0("num of columns is ", length(train_sep)))
  
  #
  print("writing csv ...")
  fwrite(train_sep,
         file = paste0("/home/paperspace/data/talkingdata/train_sep_", ii, "_added.csv"),
         nThread = 8)
  
  if(ii == 9) {
    print("creating train index ...")
    library(caret)
    set.seed(1)
    train_sep <- train_sep[hour %in% c(4:5, 9:10, 13:14)]
    train.index <- createDataPartition(
      train_sep$is_attributed,
      p = 0.85, list = FALSE)
    saveRDS(train.index, "train_index_9_4.5_9.10_13.14.rds")
  }
  
  
}

## add validation
