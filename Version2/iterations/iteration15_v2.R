library(data.table)
#load data
data <- data.table::fread(input = "../training_set_VU_DM_2014.csv", na.strings = c("NA", "NULL", "null"), data.table = FALSE)

#preprocess data
source("Version2/data_scripts_v2/iteration15_v2.R")
data_new <- iteration15(data, 0.8)

#train xgboost
library(xgboost)

train <- data_new$train[order(data_new$train$srch_id),]
counts <- data.frame(table(train$srch_id))

dtrain <- xgb.DMatrix(as.matrix(train[,-which(colnames(train) %in% c("srch_id", "target"))]), label = train$target, group = counts$Freq)
dtest <- xgb.DMatrix(as.matrix(data_new$test[,-which(colnames(data_new$test) %in% c("srch_id", "target"))]))

#listwise model (lambdaMart)
bst <- xgb.train(data = dtrain, max.depth = 7, eta = 0.001, nthread = 7, nround = 1000, objective = "rank:pairwise", 
                 eval_metric = "ndcg", verbose = 3)

#calculate variable importance and create graph
importance_matrix <- xgb.importance(colnames(dtrain),model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#Calculate NDCG test set
predictions <- predict(bst,dtest)
id <- data_new$test$srch_id
label <- data_new$test$target

source("Version2/ndcg_function.R")
metric <- ndcg(label, id, predictions, 38)


#Calculate NDCG train set
predictions <- predict(bst,dtrain)
id <- train$srch_id
label <- train$target

source("Version2/ndcg_function.R")
metric <- ndcg(label, id, predictions, 38)
