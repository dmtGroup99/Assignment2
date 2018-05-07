library(data.table)
#train data exist out of about 4.9 million instances and 54 variables
data <- data.table::fread(input = "../training_set_VU_DM_2014.csv", na.strings = c("NA", "NULL", "null"))

#See immediately many NULL/missing values. Thus lets check this out
head(data, n=10)

#Thus 20 variables where 80% of data is missing
missing_percentage <- apply(data, 2, function(col) sum(is.na(col))/length(col))
hist(missing_percentage)
sum(missing_percentage>=0.8)

#20 variables higher than 80% of data missing
#lets throw these variables away, because to much is missing
#maybe more can be done with this values in later iterations
names(missing_percentage[missing_percentage>=0.8])

#This will give 34 variables
keep <- names(missing_percentage[missing_percentage<0.8])
data_ver2 <- subset(data, select = keep)

#split data into numerical and nominal values (check this!)
num <- data_ver2[,c("prop_review_score","prop_location_score1","prop_location_score2", "prop_log_historical_price","price_usd",
                    "srch_length_of_stay","srch_booking_window","srch_adults_count","srch_children_count",
                    "srch_room_count","orig_destination_distance")]

nom <- data_ver2[,c("srch_id", "site_id", "visitor_location_country_id", "prop_country_id", "prop_id", "prop_starrating",
                    "prop_brand_bool", "promotion_flag", "srch_destination_id", "srch_saturday_night_bool", "random_bool",
                    "comp2_rate", "comp2_inv", "comp3_rate", "comp3_inv", "comp5_rate", "comp5_inv", "comp8_rate", "comp8_inv")]

#Assign lowest value for prop_location_score2 (is stated on Kaggle, not yet done)
#Now lets fill numerical missing values by median of columns, because this is a more robust estimator and for categorical
#the one that occur most often
myNum <- function(x){
  x[is.na(x)] <- median(x, na.rm = TRUE)
  x
}
myNom <- function(x){
  x[is.na(x)] <- names(which.max(table(x)))
  x
}

num2 <- num[, lapply(.SD, myNum)]
nom2 <- nom[, lapply(.SD, myNom)]
#make numeric
#num2 <- as.data.frame(lapply(num2, function(x) as.numeric(as.character(x))))
nom2 <- as.data.frame(lapply(nom2, function(x) as.numeric(as.character(x))))

#nothing is yet done with time variable
date_time <- as.Date(data_ver2$date_time)

#give relevance scores (clicked -> 1, booked -> 5 and other -> 0)
target <- data_ver2$booking_bool + data_ver2$click_bool
target[target==2] <- 5

#very imbalanced data
hist(target)

#221879 positive examples
#sum(target[target==1])

#target <- as.factor(target)

#Much data thus use downsampling for imbalanced target and get about 400k instances which is balanced
#Or upsampling or use something like scale_pos_weight in XGBoost
#know that searchId is still in data. But is necessary to destinct the clicks and books for test set in order
#to calculate performance metric correctly
#library(caret)
X <- cbind(num2, nom2, target)
#instances <- downSample(X, target, list = FALSE, yname = "Class")

#keep_id <- X[X$target==5]$srch_id
#new_X <- subset(X, srch_id %in% keep_id)
new_X <- X
keep_id <- unique(X$srch_id)

library(xgboost)

#amount unique search id's
#length(unique(instances$srch_id))

#instances <- instances[,-which(names(instances) %in% c("srch_id"))]
#split data random into train and test
#dt = sort(sample(keep_id, keep_id*.8))
dt <- sample(keep_id, 0.8*length(keep_id))

train <- subset(new_X, srch_id %in% dt)
test <- subset(new_X, !(srch_id %in% dt))

train <- train[order(train$srch_id),]
counts <- data.frame(table(train$srch_id))

##for cv, but does not work
#a <- unique(train$srch_id)
#t <- split(a, ceiling(seq_along(a)/27678))
#custom.folds <- vector("list", length = 4)
#for(i in 1:4){
#  custom.folds[[i]] <- which(train$srch_id %in% t[[i]])
#}

dtrain <- xgb.DMatrix(as.matrix(train[,-c(12,31)]), label = train$target, group = counts$Freq)
dtest <- xgb.DMatrix(as.matrix(test[,-c(12,31)]))

#listwise model (lambdaMart)
bst <- xgb.train(data = dtrain, max.depth = 5, eta = 1, nthread = 5, nround = 100, objective = "rank:pairwise", 
                 eval_metric = "ndcg", verbose = 3)

#bst <- xgb.cv(data = dtrain, max.depth = 5, eta = 1, nthread = 5, nround = 100, objective = "rank:pairwise", 
#                 eval_metric = "ndcg", folds = custom.folds, verbose = 3)

importance_matrix <- xgb.importance(colnames(dtrain),model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#benchmark approx 0.50
#Calculate NDCG
predictions <- predict(bst,dtest)
id <- test$srch_id
label <- test$target

source("ndcg_function.R")
metric <- ndcg(label, id, predictions, 38)

d <- data.frame(relevance = label, id=id, order = predictions, one=1)
d <- d[order(d$id, -d$relevance),]
d$co<-with(d, unlist(tapply(one, id, cumsum)))
d$v<-with(d, (2^relevance-1)/(log(co+1, 2)))
dcg0<-with(d[d$co<=38,], unlist(tapply(v, id, sum)))
d<-d[order(d$id, -d$order),]
d$co<-with(d, unlist(tapply(one, id, cumsum)))
d$v<-with(d, (2^relevance-1)/(log(co+1, 2)))
dcg<-with(d[d$co<=38,], unlist(tapply(v, id, sum)))
mean(dcg/dcg0)

#feature engineering
#calculating differences with respect to mean of search query numerical values
propscore1diff <- with(X, unlist(tapply(prop_location_score1, srch_id, function(x) x-mean(x))))
propscore2diff <- with(X, unlist(tapply(prop_location_score2, srch_id, function(x) x-mean(x))))
propreviewscorediff <- with(X, unlist(tapply(prop_review_score, srch_id, function(x) x-mean(x))))
propstarratingdiff <- with(X, unlist(tapply(prop_starrating, srch_id, function(x) x-mean(x))))
priceusddiff <- with(X, unlist(tapply(price_usd, srch_id, function(x) x-mean(x))))

X2 <- cbind(propscore1diff, propscore2diff, propreviewscorediff, propstarratingdiff, priceusddiff, X)

keep_id <- X2[X2$target==5]$srch_id
new_X2 <- subset(X2, srch_id %in% keep_id)

dt <- sample(keep_id, 0.8*length(keep_id))

train <- subset(new_X2, srch_id %in% dt)
test <- subset(new_X2, !(srch_id %in% dt))

train <- train[order(train$srch_id),]
counts <- data.frame(table(train$srch_id))

dtrain <- xgb.DMatrix(as.matrix(train[,-c(17,36)]), label = train$target, group = counts$Freq)
dtest <- xgb.DMatrix(as.matrix(test[,-c(17,36)]))

#listwise model (lambdaMart)
#approx 0.471, thus not much improvement
bst <- xgb.train(data = dtrain, max.depth = 5, eta = 1, nthread = 5, nround = 100, objective = "rank:pairwise", 
                 eval_metric = "ndcg", verbose = 3)

importance_matrix <- xgb.importance(colnames(dtrain),model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)

#month
#popularity of hotel
#stardiff
#propscore2diff (important)
#estimate position (important)