pacman::p_load(tidyverse, data.table, lightgbm, caret)
options(scipen = 9999, warn = -1, digits= 4)


##
train  <- fread("tmp/train_added.csv",
                drop = c("day", "minute", "ip"),
                colClasses = list("numeric" = 1:23))

# check all columns are double
train %>% 
  map_lgl(is.double) %>% 
  all()


## prepare modeling
print("Prepare data for modeling")
set.seed(1)

val_size <- 2500000

valid <- train[(nrow(train)-val_size+1):nrow(train)]
train <- train[1:(nrow(train)-val_size)]

rm(val_size)
gc(T)

#
cat("train size : ", dim(train), "\n")
cat("valid size : ", dim(valid), "\n")

#
categorical_features = c("app", "device", "os", "channel", "hour"#,
                         #"ip",
                         #"minute"
                         )

# continuous variables must be numeric!!!!
var_set <- colnames(train[, -"is_attributed"])

dtrain = lgb.Dataset(data = data.matrix(
  train[, var_set, with = F]), 
  label = train$is_attributed, categorical_feature = categorical_features)

#
cat("Creating the 'dvalid' for modeling...")
dvalid = lgb.Dataset(
  data = data.matrix(valid[, var_set, with = F]),
  label = valid$is_attributed, 
  categorical_feature = categorical_features)

#
rm(train, valid, train.index)
invisible(gc())

## modeling
print("Modelling")
params = list(objective = "binary", 
              tree_learner="data",
              metric = "auc", 
              learning_rate= 0.1,
              num_leaves= 7,
              max_depth= 3,
              min_child_samples= 100,
              max_bin= 100,
              subsample= 0.7, 
              subsample_freq= 1,
              colsample_bytree= 0.9,
              min_child_weight= 0,
#              min_split_gain= 0,
              scale_pos_weight= 200)

# train
model <- lgb.train(params, dtrain,
                   valids = list(validation = dvalid),
                   nthread = 8,
                   nrounds = 1000,
                   verbose= 1, 
                   early_stopping_rounds = 50,
                   eval_freq = 25)

##
rm(dtrain, dvalid)
invisible(gc())


cat("Validation AUC @ best iter: ",
    max(unlist(model$record_evals[["validation"]][["auc"]][["eval"]])),
    "\n\n")
# 0.9873 

## save
lgb.save(model, "mod/lgb_v8")
#model <- lgb.load("mod/lgb_v8")


## evaluate test
test <- fread("tmp/test_added.csv",
              drop = c("day", "ip", "minute"))


##
cat("Setting up the submission file... \n")
sub <- data.table(click_id = test$click_id, is_attributed = NA) 
test$click_id <- NULL
invisible(gc())

##
preds <- predict(model, 
                 data = data.matrix(test[, var_set, with = F]),
                 n = model$best_iter)

head(preds)


##
preds <- as.data.frame(preds)
sub$is_attributed = preds
rm(test)
invisible(gc())

##
cat("Rounding: \n")
sub$is_attributed = round(sub$is_attributed, 4)

##
cat("Writing into a csv file: \n")
fwrite(sub, "sub/lightgbm_r_v8.csv", nThread = 8)



##
cat("Feature importance: ")
lgb.importance(model, percentage = TRUE)
