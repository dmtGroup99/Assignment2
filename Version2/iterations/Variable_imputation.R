library("glmnet")
library("VIM")
library("dplyr")
impute_review_score <- function(df) {
  var <- c("prop_review_score")
  vars_dist <- c('prop_starrating', 'prop_brand_bool', 'prop_location_score1', 'prop_log_historical_price')
  subset <- select(df, vars_dist,var)
  start_time <- Sys.time()
  subset <- kNN(subset, variable = var,dist_var = vars_dist, k=5)#, numFun = weightedMean, weightDist=TRUE)# variables = c('prop_location_score2', 'prop_review_score', 'orig_destination_distance'))
  end_time <- Sys.time()
  runtime_RS <- end_time - start_time #500000 obs --> 5.12 min
  print(runtime_RS)
  df$prop_review_score <- subset$prop_review_score
  return(df)
}

impute_review_score_k <- function(df, k) {
  var <- c("prop_review_score")
  vars_dist <- c('prop_starrating', 'prop_brand_bool', 'prop_location_score1', 'prop_log_historical_price')
  df_small <- select(df, vars_dist,var)
  prop_review_score_imp <- NULL
  for (i in 1:(k-1)) {
    print(i)
    step_size <- floor(nrow(df_small)/k)
    subset <- slice(df_small,(((i-1)*step_size)+1):(i*step_size))
    start_time <- Sys.time()
    subset <- kNN(subset, variable = var,dist_var = vars_dist, k=5)#, numFun = weightedMean, weightDist=TRUE)# variables = c('prop_location_score2', 'prop_review_score', 'orig_destination_distance'))
    prop_review_score_imp <- rbind(prop_review_score_imp, as.matrix(subset$prop_review_score))
  }
  subset <- slice(df_small,(((k-1)*step_size)+1):nrow(df_small))
  subset <- kNN(subset, variable = var,dist_var = vars_dist, k=5)#, numFun = weightedMean, weightDist=TRUE)# variables = c('prop_location_score2', 'prop_review_score', 'orig_destination_distance'))
  prop_review_score_imp <- rbind(prop_review_score_imp, as.matrix(subset$prop_review_score))
  print(nrow(prop_review_score_imp))
  print(nrow(df))
  df$prop_review_score <- prop_review_score_imp
  return(df)
}

impute_prop_location_score2 <- function(df) {
  df$prop_location_score2 <- sapply(df$prop_location_score2, as.numeric)
  vars_predictors <- c('prop_location_score1', 'prop_brand_bool', 'position', 'prop_starrating')
  subset <- select(df, prop_location_score2, vars_predictors)
  subset$ID <- seq.int(nrow(subset))
  targetdata <- subset[is.na(subset$prop_location_score2),]
  traindata <- subset[!is.na(subset$prop_location_score2),]
  hist(traindata$prop_location_score2, breaks=100)
  y_train <- as.matrix(log(traindata$prop_location_score2+0.0000001)) 
  #hist(y_train, breaks = 100)
  X_train <- as.matrix(traindata[,-which(colnames(traindata) %in% c('prop_location_score2', 'ID'))])
  cvfit <- cv.glmnet(X_train, y_train, type.measure = 'mse')
  #plot(cvfit)
  X_target <- as.matrix(targetdata[, -which(colnames(traindata) %in% c('prop_location_score2', 'ID'))])
  logy_hat <- predict(cvfit, newx=X_target, s = "lambda.min")
  #hist(logy_hat, breaks=100)
  y_hat <- exp(logy_hat)
  hist(y_hat, breaks=100)
  hist(traindata$prop_location_score2, breaks=100)
  targetdata$prop_location_score2 <- y_hat
  targetdata <- do.call(data.frame, targetdata)
  colnames(targetdata)[1] <- "prop_location_score2"
  subset <- rbind(targetdata, traindata)
  subset <- subset[order(subset$ID),]
  df$prop_location_score2 <- subset$prop_location_score2
  return(df)
  
}

