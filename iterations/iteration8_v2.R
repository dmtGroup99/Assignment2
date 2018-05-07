library(data.table)
#load data
#data <- data.table::fread(input = "../training_set_VU_DM_2014.csv", na.strings = c("NA", "NULL", "null"), data.table = FALSE)
data <- sample_n(data_train, 500000)
#preprocess data
setwd("C:/Users/Vincent/Documents/GitHub/Assignment2")
source("data_scripts_v2/iteration8_v2.R")
data_new <- iteration8_v2(data, 0.8)

#train xgboost
library(xgboost)

train <- data_new$train[order(data_new$train$srch_id),]
counts <- data.frame(table(data_new$train$srch_id))

dtrain <- xgb.DMatrix(as.matrix(train[,-c("srch_id", "target")]), label = train$target, group = counts$Freq) #without srch_id, target 
dtest <- xgb.DMatrix(as.matrix(data_new$test[,-c("srch_id", "target")]))

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
