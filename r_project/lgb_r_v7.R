if (!require("pacman")) install.packages("pacman")
pacman::p_load(knitr, tidyverse, data.table, lubridate, zoo, DescTools, lightgbm)
set.seed(84)               
options(scipen = 9999, warn = -1, digits= 4)

##
val_ratio <- 0.9

##
train_path <- "/home/paperspace/data/talkingdata/train_sep_9_added.csv"
test_path  <- "/home/paperspace/data/talkingdata/test.csv"


##
cat("Reading the training data...\n")
train <- fread(train_path, drop = c("day"),#, "ip"),
               colClasses = list(numeric=9:15)
)

## restrict time as test data
train <- train[hour %in% 4:14]

## add new variables
train[, time_in_m :=hour*60 + minute]
train[, delta_in_m := lag(time_in_m) - time_in_m, 
      by = .(ip, app, device, os, channel)]
train[is.na(delta_in_m), delta_in_m := 0]
train[, nth_click := .I,
      by = .(ip, app, device, os, channel)]

## Modelling
print("Prepare data for modeling")
library(caret)
train.index <- createDataPartition(
  train$is_attributed,
  p = val_ratio, list = FALSE)


#
dtrain <- train[ train.index,]
valid  <- train[-train.index,]

rm(train)
invisible(gc())

#
cat("train size : ", dim(dtrain), "\n")
cat("valid size : ", dim(valid), "\n")

#
categorical_features = c("app", "device", "os", "channel", "hour", "ip")

#
cat("Creating the 'dtrain' for modeling...")


## continuous variables must be numeric!!!!
dtrain = lgb.Dataset(data = data.matrix(
  dtrain %>% 
    select(-is_attributed)
), 
label = dtrain$is_attributed, categorical_feature = categorical_features)

#
cat("Creating the 'dvalid' for modeling...")
dvalid = lgb.Dataset(
  data = data.matrix(valid %>% 
                       select(-is_attributed)), 
  label = valid$is_attributed, 
  categorical_feature = categorical_features)

#######################################################

rm(valid, train.index)
invisible(gc())

#######################################################

print("Modelling")
params = list(objective = "binary", 
              tree_learner="data",
              metric = "auc", 
              learning_rate= 0.1,
              num_leaves= 7,
              max_depth= 4,
              min_child_samples= 100,
              max_bin= 100,
              subsample= 0.7, 
              subsample_freq= 1,
              colsample_bytree= 0.7,
              min_child_weight= 0,
              min_split_gain= 0,
              scale_pos_weight= 200)

##
model <- lgb.train(params, dtrain,
                   valids = list(validation = dvalid),
                   nthread = 8,
                   nrounds = 1500,
                   verbose= 1, 
                   early_stopping_rounds = 50,
                   eval_freq = 25)

##
rm(dtrain, dvalid)
invisible(gc())


cat("Validation AUC @ best iter: ",
    max(unlist(model$record_evals[["validation"]][["auc"]][["eval"]])),
    "\n\n")
# 0.9742

## save
lgb.save(model, "mod/lgb_v7")
model <- lgb.load("mod/lgb_v7")

# Preparing the test data

##
#cat("Reading the test data: ", test_rows, " rows. \n")
test <- fread("/home/paperspace/data/talkingdata/test_added.csv",
              drop = c("day"),
              colClasses = list(numeric=9:15))
              
## add new variables
test[, time_in_m :=hour*60 + minute]
test[, delta_in_m := lag(time_in_m) - time_in_m, 
      by = .(ip, app, device, os, channel)]
test[is.na(delta_in_m), delta_in_m := 0]
test[, nth_click := .I,
      by = .(ip, app, device, os, channel)]


              

              
##
cat("Setting up the submission file... \n")
sub <- data.table(click_id = test$click_id, is_attributed = NA) 
test$click_id <- NULL
invisible(gc())

##
cat("The test set has", nrow(test), "rows and", ncol(test), "columns.\n")
cat("The column names of the test set are: \n")
cat(colnames(test), "\n")
print("The size of the test set is: ") 
print(object.size(test), units = "auto")

##
cat("Predictions: \n")
preds <- predict(model, 
                 data = data.matrix(test),
                 n = model$best_iter)

#######################################################

cat("Converting to data frame: \n")
preds <- as.data.frame(preds)

#######################################################

cat("Creating the submission data: \n")
sub$is_attributed = preds

#######################################################

cat("Removing test... \n")
rm(test)
invisible(gc())

#######################################################

cat("Rounding: \n")
sub$is_attributed = round(sub$is_attributed, 4)

#######################################################

cat("Writing into a csv file: \n")
fwrite(sub, "sub/lightgbm_r_v7.csv")

#####
cat("A quick peek at the submission data: \n") 
head(sub, 10)


#####

cat("Feature importance: ")
kable(lgb.importance(model, percentage = TRUE))


cat("\nAll done!..")
