iteration20 <- function(data, train_instances){
  keep_id <- unique(data$srch_id)
  train_instances <- 0.8
  #split data random into train and test
  dt <- sample(keep_id, train_instances*length(keep_id))
  
  train <- subset(data, srch_id %in% dt)
  test <- subset(data, !(srch_id %in% dt))
  
  #Impute missing values for review score (using kNN) and prop_location_score2 (based on loglinear lm)
  source('Variable_imputation.R')
  train <- impute_review_score(train) #If this one does not work, then use impute_review_score_k
  #train <- impute_review_score_k(train, k=50) #Perform kNN in k steps (The lower the k, the better)
  train <- impute_prop_location_score2(train)
  test <- impute_review_score(test) #If this one does not work, then use impute_review_score_k
  #test <- impute_review_score_k(test, k=100) #Perform kNN in k steps (The lower the k, the better)
  test <- impute_prop_location_score2(test)
  
  
  
  #split data into numerical and nominal values (check this!)
  num <- train[,c("prop_review_score","prop_location_score1","prop_location_score2", "prop_log_historical_price","price_usd",
                  "srch_length_of_stay","srch_booking_window","srch_adults_count","srch_children_count",
                  "srch_room_count","orig_destination_distance", "position")]
  
  visit_star <- train[,c("visitor_hist_adr_usd")]
  visit_usd <- train[,c("visitor_hist_adr_usd")]
  
  visit_star[is.na(visit_star)] <- 0
  visit_usd[is.na(visit_usd)] <- 0
  
  nom <- train[,c("srch_id", "site_id", "visitor_location_country_id", "prop_country_id", "prop_id", "prop_starrating",
                  "prop_brand_bool", "promotion_flag", "srch_destination_id", "srch_saturday_night_bool", "random_bool",
                  "comp2_rate", "comp2_inv", "comp3_rate", "comp3_inv", "comp5_rate", "comp5_inv", "comp8_rate", "comp8_inv")]
  
  num_review <- train[,c("prop_review_score")]
  
  num_median <- apply(num, 2, function(x) median(x, na.rm = TRUE))
  nom_occur <- apply(nom, 2, function(x) names(which.max(table(x))))
  
  numeric_median <- function(x, num_median){
    for(i in 1:length(x[1,])){
      x[,i][is.na(x[,i])] <- unname(num_median[i])
    }
    return(x)
  }
  
  nominal_occurence <- function(x, nom_occur){
    for(i in 1:length(x[1,])){ 
      x[,i][is.na(x[,i])] <- unname(nom_occur[i])
    }
    return(x)
  }
  
  num <- numeric_median(num, num_median)
  nom <- nominal_occurence(nom, nom_occur)
  
  #make numeric
  nom <- as.data.frame(lapply(nom, function(x) as.numeric(as.character(x))))
  
  #give relevance scores (clicked -> 1, booked -> 5 and other -> 0)
  target <- train$booking_bool + train$click_bool
  target[target==2] <- 5
  
  train_set <- cbind(num, nom, target)
  
  positionmedian <- with(train_set, tapply(position, prop_id, function(x) median(x)))
  
  key <- data.frame(median_postition = unname(positionmedian), id = names(positionmedian))
  
  train_set$estimate_position <- key[match(train_set$prop_id, key$id), "median_postition"]
  
  train_set$position <- NULL
  
  #feature engineering
  #calculating differences with respect to mean of search query numerical values
  train_set$propscore1diff <- with(train_set, unlist(tapply(prop_location_score1, srch_id, function(x) x-median(x))))
  train_set$propscore2diff <- with(train_set, unlist(tapply(prop_location_score2, srch_id, function(x) x-median(x))))
  train_set$propreviewscorediff <- with(train_set, unlist(tapply(prop_review_score, srch_id, function(x) x-median(x))))
  train_set$propstarratingdiff <- with(train_set, unlist(tapply(prop_starrating, srch_id, function(x) x-median(x))))
  train_set$priceusddiff <- with(train_set, unlist(tapply(price_usd, srch_id, function(x) x-median(x))))
  train_set$visitusddiff <- train_set$price_usd - visit_usd
  train_set$visitstardiff <- train_set$prop_starrating - visit_star

  #split data into numerical and nominal values (check this!)
  num <- test[,c("prop_review_score","prop_location_score1","prop_location_score2", "prop_log_historical_price","price_usd",
                 "srch_length_of_stay","srch_booking_window","srch_adults_count","srch_children_count",
                 "srch_room_count","orig_destination_distance")]
  
  visit_star <- test[,c("visitor_hist_adr_usd")]
  visit_usd <- test[,c("visitor_hist_adr_usd")]
  
  visit_star[is.na(visit_star)] <- 0
  visit_usd[is.na(visit_usd)] <- 0
  
  nom <- test[,c("srch_id", "site_id", "visitor_location_country_id", "prop_country_id", "prop_id", "prop_starrating",
                 "prop_brand_bool", "promotion_flag", "srch_destination_id", "srch_saturday_night_bool", "random_bool",
                 "comp2_rate", "comp2_inv", "comp3_rate", "comp3_inv", "comp5_rate", "comp5_inv", "comp8_rate", "comp8_inv")]
  
  num <- numeric_median(num, num_median)
  nom <- nominal_occurence(nom, nom_occur)
  
  #make numeric
  nom <- as.data.frame(lapply(nom, function(x) as.numeric(as.character(x))))
  
  #give relevance scores (clicked -> 1, booked -> 5 and other -> 0)
  target <- test$booking_bool + test$click_bool
  target[target==2] <- 5
  
  test_set <- cbind(num, nom, target)
  
  
  d <- subset(test_set, !(test_set$prop_id %in% train_set$prop_id))
  key2 <- data.frame(median_postition = median(positionmedian), id = as.factor(unique(d$prop_id)))
  
  key_final <- rbind(key,key2)
  #key_final <- subset(key_final, test_set$prop_id %in% key_final$id)
  test_set$estimate_position <- key_final[match(test_set$prop_id, key_final$id), "median_postition"]
  
  test_set$position <- NULL
  
  test_set$propscore1diff <- with(test_set, unlist(tapply(prop_location_score1, srch_id, function(x) x-median(x))))
  test_set$propscore2diff <- with(test_set, unlist(tapply(prop_location_score2, srch_id, function(x) x-median(x))))
  test_set$propreviewscorediff <- with(test_set, unlist(tapply(prop_review_score, srch_id, function(x) x-median(x))))
  test_set$propstarratingdiff <- with(test_set, unlist(tapply(prop_starrating, srch_id, function(x) x-median(x))))
  test_set$priceusddiff <- with(test_set, unlist(tapply(price_usd, srch_id, function(x) x-median(x))))
  test_set$visitusddiff <- test_set$price_usd - visit_usd
  test_set$visitstardiff <- test_set$prop_starrating - visit_star
  
  out <- list()
  out$train <- train_set
  out$test <- test_set
  return(out)
}