library(data.table)
#load data
data2 <- data.table::fread(input = "../test.csv", na.strings = c("NA", "NULL", "null"), data.table = FALSE)

test <- data2

#split data into numerical and nominal values (check this!)
num <- test[,c("prop_review_score","prop_location_score1","prop_location_score2", "prop_log_historical_price","price_usd",
               "srch_booking_window",
               "orig_destination_distance")]

nom <- test[,c("srch_id", "site_id","prop_country_id", "prop_id", "prop_starrating",
               "prop_brand_bool", "promotion_flag", "srch_destination_id", "random_bool")]

num <- numeric_median(num, num_median)
nom <- nominal_occurence(nom, nom_occur)

#make numeric
nom <- as.data.frame(lapply(nom, function(x) as.numeric(as.character(x))))

test_set <- cbind(num, nom)

d <- subset(test_set, !(test_set$prop_id %in% train_set$prop_id))
key2 <- data.frame(median_postition = median(positionmedian), id = as.factor(unique(d$prop_id)))

key_final <- rbind(key,key2)
#key_final <- subset(key_final, test_set$prop_id %in% key_final$id)
test_set$estimate_position <- key_final[match(test_set$prop_id, key_final$id), "median_postition"]

test_set$propscore1diff <- with(test_set, unlist(tapply(prop_location_score1, srch_id, function(x) x-median(x))))
test_set$propscore2diff <- with(test_set, unlist(tapply(prop_location_score2, srch_id, function(x) x-median(x))))
test_set$propreviewscorediff <- with(test_set, unlist(tapply(prop_review_score, srch_id, function(x) x-median(x))))
test_set$propstarratingdiff <- with(test_set, unlist(tapply(prop_starrating, srch_id, function(x) x-median(x))))
test_set$priceusddiff <- with(test_set, unlist(tapply(price_usd, srch_id, function(x) x-median(x))))


dtest <- xgb.DMatrix(as.matrix(test_set[,-c(8)]))

predictions <- predict(bst,dtest)
srch_id <- test_set$srch_id
prop_id <- test_set$prop_id

pred <- data.frame(SearchId=srch_id, PropertyId=test_set$prop_id, predictions=predictions)
pred <- pred[order(pred$SearchId, -pred$predictions),]

write.csv(pred[,c(1,2)], file = "kaggle_pred.csv", row.names=FALSE)
