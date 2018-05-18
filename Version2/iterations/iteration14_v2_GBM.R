library(gbm)
#https://rdrr.io/cran/gbm/src/demo/pairwise.R
#https://pdfs.semanticscholar.org/a3f6/d964ac323b87d2de3434b23444cb774a216e.pdf

#is slower than xgb. However, cv is possible
#pairwise = ranking measure using the LambdaMart algorithm
#https://cran.r-project.org/web/packages/gbm/gbm.pdf


library(data.table)
#load data
data <- data.table::fread(input = "../training_set_VU_DM_2014.csv", na.strings = c("NA", "NULL", "null"), data.table = FALSE)

#preprocess data
source("Version2/data_scripts_v2/iteration14_v2.R")
data_new <- iteration14(data, 0.8)

dtrain <- data_new$train
dtest <- data_new$test[,-which(colnames(data_new$test) %in% c("target"))]

gbm.ndcg <- gbm(target~., # formula
                data=dtrain,     # dataset
                distribution=list(   # loss function:
                  name='pairwise',   # pairwise
                  metric="ndcg",     # ranking metric: normalized discounted cumulative gain
                  group='srch_id'),  # column indicating query groups
                n.trees=3000,        # number of trees
                shrinkage=0.01,      # learning rate
                interaction.depth=6, # number per splits per tree
                bag.fraction = 0.5,  # subsampling fraction
                train.fraction = 1,  # fraction of data for training
                n.minobsinnode = 10, # minimum number of obs for split
                keep.data=TRUE,      # store copy of input data in model
                cv.folds=3,          # number of cross validation folds
                verbose = TRUE,      # print progress
                n.cores = 7)         # use a single core

# cv results. gives best # trees. NDCG is probably inverse.
best.iter.ndcg <- gbm.perf(gbm.ndcg, method='cv')

#relative influence
summary(gbm.ndcg, n.trees=best.iter.ndcg, main='pairwise (ndcg)')

#make pred for test set
predictions <- predict(gbm.ndcg, dtest, best.iter.ndcg)

## does not work!
#N <- length(predictions)
#ndcg5.loss=sapply(1:length(predictions), FUN=function(i) {
#  gbm.loss(y=test$target, predictions[[i]], w=rep(1,N), offset=NA, dist=list(name='pairwise', metric="ndcg"),
#           baseline=0, group=test$srch_id, max.rank=38) })

## does work
id <- data_new$test$srch_id
label <- data_new$test$target

source("Version2/ndcg_function.R")
metric <- ndcg(label, id, predictions, 38)
