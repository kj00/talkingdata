pacman::p_load(knitr, tidyverse, data.table, lubridate, zoo, DescTools, lightgbm)
options(scipen = 9999, warn = -1, digits= 4)

train_path <- "/home/paperspace/data/talkingdata/train_sep_9_added.csv"

##
train <- fread(train_path,
              colClasses = list(numeric = 1:30),
               drop = c("day", "ip", "minute"))

train  %>% 
  map_lgl(is.numeric) %>% 
  all()


##
print("Prepare data for modeling")

train.index <- readRDS("train_index_9_4.5_9.10_13.14.rds")

dtrain <- rbind(train[hour %in% c(4:5, 9:10, 13:14)][train.index],
                train[hour %in% c(6:8, 11:12, 15, 22:23)])

valid  <- train[hour %in% c(4:5, 9:10, 13:14)][-train.index]


rm(train)
invisible(gc())

#
cat("train size : ", dim(dtrain), "\n")
cat("valid size : ", dim(valid), "\n")

#
categorical_features = c("app", "device", "os", "channel", "hour"#,
                         #"ip",
                         #"minute"
                         )

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
# 0.9761!?

## save
#lgb.save(model, "mod/lgb_v9")
#model <- lgb.load("mod/lgb_v8")

## dont need ip?

test <- fread("/home/paperspace/data/talkingdata/test_added.csv",
              drop = c("day", "ip", "minute"))

#test[, minute := floor(minute*0.1)]


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
head(preds)
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
fwrite(sub, "sub/lightgbm_r_v9.csv", nThread = 8)



##
cat("Feature importance: ")
kable(lgb.importance(model, percentage = TRUE))

