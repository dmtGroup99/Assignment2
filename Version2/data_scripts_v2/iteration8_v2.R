iteration8_v2 <- function(data, train_instances){
  
  
  ##Visitor historical booking information: visitor_hist_adr_usd, visitor_hist_starrating
  #--> visitor_hist_adr_usd, fill NA with median (95%)
  #--> visitor_hist_starrating, fill NA with median (95%)
  #--> create dummy variable indicating whether a visitor had a previous booking
  data$visitor_hist_adr_usd <- sapply(data$visitor_hist_adr_usd, as.numeric)
  data$visitor_hist_starrating <- sapply(data$visitor_hist_starrating, as.numeric)
  data$visitor_hist_dummy <- ifelse(is.na(data$visitor_hist_adr_usd), 1, 0)
  data$visitor_hist_adr_usd[is.na(data$visitor_hist_adr_usd)] <- median(data$visitor_hist_adr_usd[!is.na(data$visitor_hist_adr_usd)])
  data$visitor_hist_starrating[is.na(data$visitor_hist_starrating)] <- median(data$visitor_hist_starrating[!is.na(data$visitor_hist_starrating)])
  
  ##Property information: prop_starrating, prop_review_score, prop_brand_bool, prop_location_score1, prop_location_score2
  #--> prop_starrating, no preprocessing/cleaning needed
  #--> prop_review_score, 7364 NA values (0.15%) replaced with median
  #--> prop_brand_bool, no preprocessing/cleaning needed
  #--> prop_location_score1, no preprocessing/cleaning needed (what to do with skewed data?)
  #--> prop_location_score2, 22% NA --> fill with median
  data$prop_review_score <- sapply(data$prop_review_score, as.numeric)
  data$prop_location_score2 <- sapply(data$prop_location_score2, as.numeric)
  data$prop_review_score[is.na(data$prop_review_score)] <- median(data$prop_review_score[!is.na(data$prop_review_score)])
  data$prop_location_score2[is.na(data$prop_location_score2)] <- median(data$prop_location_score2[!is.na(data$prop_location_score2)])
  
  ##Price information: promotion_flag, prop_log_historical_price
  #--> promotion_flag, no preprocessing/cleaning needed
  #--> prop_log_historical_price, 14% zeros --> fill with median
  #--> create dummy variable indicating if hotel has an historical booking
  data$prop_log_hist_price_dummy <- ifelse(data$prop_log_historical_price==0, 1, 0)
  data$prop_log_historical_price[data$prop_log_historical_price==0] <- median(data$prop_log_historical_price[data$prop_log_historical_price!=0])
  
  #Skip variables with many NA values --> 34 variables left
  missing_percentage <- apply(data, 2, function(col) sum(is.na(col))/length(col))
  keep <- names(missing_percentage[missing_percentage<0.5])
  data_ver2 <- subset(data, select = keep)
  
  #Drop variables with low relevance/quality: date_time, site_id, visitor_location_country_id, prop_country_id, prop_id, price_usd, 
  data_ver2$date_time <- NULL
  #data_ver2$site_id <- NULL
  #data_ver2$visitor_location_country_id <- NULL
  #data_ver2$prop_country_id <- NULL
  #data_ver2$prop_id <- NULL
  #data_ver2$price_usd <- NULL #low quality due to different meaning/currency per country..
  #data_ver2$orig_destination_distance <- NULL #low quality, many zeros and NA values..
  
  #1-hot encoding for site_id --> 33 dummies
  library(dummies)
  data_ver2<-dummy.data.frame(data_ver2,names=c("site_id"), sep = "_")
  
  #split data random into train and test
  keep_id <- unique(data_ver2$srch_id)
  dt <- sample(keep_id, train_instances*length(keep_id))
  train <- subset(data_ver2, srch_id %in% dt)
  test <- subset(data_ver2, !(srch_id %in% dt))
  
  # #split data into numerical and nominal values (check this!)
  # num <- train[,c("prop_review_score","prop_location_score1","prop_location_score2", "prop_log_historical_price","price_usd",
  #                 "srch_length_of_stay","srch_booking_window","srch_adults_count","srch_children_count",
  #                 "srch_room_count","orig_destination_distance")]
  # 
  # nom <- train[,c("srch_id", "site_id", "visitor_location_country_id", "prop_country_id", "prop_id", "prop_starrating",
  #                 "prop_brand_bool", "promotion_flag", "srch_destination_id", "srch_saturday_night_bool", "random_bool",
  #                 "comp2_rate", "comp2_inv", "comp3_rate", "comp3_inv", "comp5_rate", "comp5_inv", "comp8_rate", "comp8_inv")]
  # 
  # num_median <- apply(num, 2, function(x) median(x, na.rm = TRUE))
  # nom_occur <- apply(nom, 2, function(x) names(which.max(table(x))))
  # 
  # numeric_median <- function(x, num_median){
  #   for(i in 1:length(x[1,])){
  #     x[,i][is.na(x[,i])] <- unname(num_median[i])
  #   }
  #   return(x)
  # }
  # 
  # nominal_occurence <- function(x, nom_occur){
  #   for(i in 1:length(x[1,])){ 
  #     x[,i][is.na(x[,i])] <- unname(nom_occur[i])
  #   }
  #   return(x)
  # }
  # 
  # num <- numeric_median(num, num_median)
  # nom <- nominal_occurence(nom, nom_occur)
  # 
  # #make numeric
  # nom <- as.data.frame(lapply(nom, function(x) as.numeric(as.character(x))))
  
  #give relevance scores (clicked -> 1, booked -> 5 and other -> 0)
  target <- train$booking_bool + train$click_bool
  target[target==2] <- 5
  train$click_bool <- NULL
  train$booking_bool <- NULL
  train_set <- cbind(train, target)
  
  
  # #split data into numerical and nominal values (check this!)
  # num <- test[,c("prop_review_score","prop_location_score1","prop_location_score2", "prop_log_historical_price","price_usd",
  #                "srch_length_of_stay","srch_booking_window","srch_adults_count","srch_children_count",
  #                "srch_room_count","orig_destination_distance")]
  # 
  # nom <- test[,c("srch_id", "site_id", "visitor_location_country_id", "prop_country_id", "prop_id", "prop_starrating",
  #                "prop_brand_bool", "promotion_flag", "srch_destination_id", "srch_saturday_night_bool", "random_bool",
  #                "comp2_rate", "comp2_inv", "comp3_rate", "comp3_inv", "comp5_rate", "comp5_inv", "comp8_rate", "comp8_inv")]
  # 
  # num <- numeric_median(num, num_median)
  # nom <- nominal_occurence(nom, nom_occur)
  # 
  # #make numeric
  # nom <- as.data.frame(lapply(nom, function(x) as.numeric(as.character(x))))
  
  #give relevance scores (clicked -> 1, booked -> 5 and other -> 0)
  target <- test$booking_bool + test$click_bool
  target[target==2] <- 5
  test$click_bool <- NULL
  test$booking_bool <- NULL
  test_set <- cbind(test, target)
  
  out <- list()
  out$train <- train_set
  out$test <- test_set
  return(out)
}
