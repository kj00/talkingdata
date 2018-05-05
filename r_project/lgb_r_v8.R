pacman::p_load(tidyverse, data.table, lightgbm)
options(scipen = 9999, warn = -1, digits= 4)


##
train  <- fread("tmp/train_added.csv",
                drop = c("day", "minute", "ip"),
                colClasses = list("numeric" = 2:27))

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
categorical_features = c("app"
                         ,"device"
                         ,"os",
                         "channel"
                         ,"hour"
                         #,"ip"
                         #,"minute"
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
params = list(boosting_type="gbdt",
              objective = "binary", 
              tree_learner="data",
              metric = "auc", 
              learning_rate= 0.04,
              num_leaves= 31,
              max_depth= -1,
              min_child_samples= 20,
              max_bin= 255,
              subsample= 0.6, 
              subsample_freq= 0,
              colsample_bytree= 0.3,
              min_child_weight= 5,
              subsample_for_bin = 20000,
              min_split_gain= 0,
              reg_alpha=0.99,
              reg_lambda=0.9,
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
# 0.9797 

## save
lgb.save(model, "mod/lgb_v8")
#model <- lgb.load("mod/lgb_v8")


## evaluate test
test <- fread("tmp/test_added.csv",
              colClasses = list("numeric" = 2:27),
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
sub$is_attributed[] = preds

rm(test)
invisible(gc())

##
cat("Rounding: \n")
#sub$is_attributed = round(sub$is_attributed, 4)

##
cat("Writing into a csv file: \n")
fwrite(sub, "sub/lightgbm_r_v8.csv", nThread = 8)



##
cat("Feature importance: ")
lgb.importance(model, percentage = TRUE)
