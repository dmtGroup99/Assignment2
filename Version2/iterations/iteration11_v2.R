library(data.table)
#load data
data <- data.table::fread(input = "../training_set_VU_DM_2014.csv", na.strings = c("NA", "NULL", "null"), data.table = FALSE)

#preprocess data
source("Version2/data_scripts_v2/iteration11_v2.R")
data_new <- iteration11(data, 0.8)

#train xgboost
library(xgboost)

train <- data_new$train[order(data_new$train$srch_id),]
counts <- data.frame(table(train$srch_id))

dtrain <- xgb.DMatrix(as.matrix(train[,-which(colnames(train) %in% c("srch_id", "target"))]), label = train$target, group = counts$Freq)
#dtest <- xgb.DMatrix(as.matrix(data_new$test[,-which(colnames(data_new$test) %in% c("srch_id", "target"))]))

test <- data_new$test[order(data_new$test$srch_id),]
counts <- data.frame(table(test$srch_id))

dtest <- xgb.DMatrix(as.matrix(data_new$test[,-which(colnames(data_new$test) %in% c("srch_id", "target"))]), label = test$target, group = counts$Freq)


watchlist <- list(train=dtrain, test=dtest)

#set parallel backend
library(parallel)
library(parallelMap)
parallelStartSocket(cpus = detectCores())

bst <- xgb.train(data = dtrain, max.depth = 5, eta = 1, nthread = 7, nround = 100, objective = "rank:pairwise", 
                 watchlist=watchlist,eval_metric = "ndcg", verbose = 1)

bst <- xgb.train(data = dtrain, max.depth = 12, eta = 0.01, nthread = 7, nround = 4000, objective = "rank:pairwise", 
                 watchlist=watchlist,eval_metric = "ndcg", verbose = 1, gamma = 100)

#listwise model (lambdaMart)
#bst <- xgb.train(data = dtrain, max.depth = 10, eta = 0.01, nthread = 7, nround = 5000, objective = "rank:pairwise", 
#                 watchlist=watchlist,eval_metric = "ndcg", verbose = 1,
#                 min_child_weight = 5, gamma = 10, subsample=0.8, colsample_bytree=0.8)

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

# Creates plot of cross validation results of model training
plot(bst$evaluation_log$iter,bst$evaluation_log$train_ndcg,type="l",col="red"
     ,xlab="Tree",ylab = "NDCG",
     main = "Learning curve")
lines(bst$evaluation_log$iter,bst$evaluation_log$test_ndcg,col="green")
legend(600,0.58, c("Test","Train"),lty=c(1,1),lwd=c(2.5,2.5),col=c("green","red"))
