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
X <- cbind(num2, nom2, target, booking_bool=data_ver2$booking_bool, click_bool=data_ver2$click_bool)

number_bookings_hotel <- with(X, tapply(booking_bool, prop_id, function(x) sum(x)))
number_clicks_hotel <- with(X, tapply(click_bool, prop_id, function(x) sum(x)))
number_appereance_hotel <- with(X, tapply(target, prop_id, function(x) length(x)))

popularity = (number_bookings_hotel/number_clicks_hotel)*number_appereance_hotel
popularity[is.nan(popularity)] <- 0

key <- data.frame(popularity = unname(popularity), id = names(popularity))
X$popularity <- key[match(X$prop_id, key$id), "popularity"]
X$booking_bool <- NULL
X$click_bool <- NULL
