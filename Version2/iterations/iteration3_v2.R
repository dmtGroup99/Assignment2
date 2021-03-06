library(data.table)
#load data
data <- data.table::fread(input = "../training_set_VU_DM_2014.csv", na.strings = c("NA", "NULL", "null"), data.table = FALSE)

#preprocess data
source("data_scripts_v2/iteration3_v2.R")
data_new <- iteration3(data, 0.8)

#train xgboost
library(xgboost)

train <- data_new$train[order(data_new$train$srch_id),]
counts <- data.frame(table(train$srch_id))

dtrain <- xgb.DMatrix(as.matrix(train[,-c(12,13,32)]), label = train$target, group = counts$Freq)
dtest <- xgb.DMatrix(as.matrix(data_new$test[,-c(12,31)]))

#listwise model (lambdaMart)
bst <- xgb.train(data = dtrain, max.depth = 5, eta = 1, nthread = 5, nround = 100, objective = "rank:pairwise", 
                 eval_metric = "ndcg", verbose = 3)

#calculate variable importance and create graph
importance_matrix <- xgb.importance(colnames(dtrain),model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#Calculate NDCG
predictions <- predict(bst,dtest)
id <- data_new$test$srch_id
label <- data_new$test$target

source("ndcg_function.R")
metric <- ndcg(label, id, predictions, 38)
