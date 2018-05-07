library(gbm)
#https://rdrr.io/cran/gbm/src/demo/pairwise.R
#https://pdfs.semanticscholar.org/a3f6/d964ac323b87d2de3434b23444cb774a216e.pdf

#is slower than xgb. However, cv is possible
#pairwise = ranking measure using the LambdaMart algorithm
#https://cran.r-project.org/web/packages/gbm/gbm.pdf
gbm.ndcg <- gbm(target~prop_review_score + prop_location_score1 + prop_location_score2 + prop_log_historical_price + price_usd +
                srch_length_of_stay + srch_booking_window + srch_adults_count + srch_children_count +
                srch_room_count + orig_destination_distance +
                site_id + visitor_location_country_id + prop_country_id + prop_id + prop_starrating +
                prop_brand_bool + promotion_flag + srch_destination_id + srch_saturday_night_bool + random_bool +
                comp2_rate + comp2_inv + comp3_rate + comp3_inv + comp5_rate + comp5_inv + comp8_rate + comp8_inv, # formula
                data=train,     # dataset
                distribution=list(   # loss function:
                  name='pairwise',   # pairwise
                  metric="ndcg",     # ranking metric: normalized discounted cumulative gain
                  group='srch_id'),  # column indicating query groups
                n.trees=100,         # number of trees
                shrinkage=1,         # learning rate
                interaction.depth=3, # number per splits per tree
                bag.fraction = 0.5,  # subsampling fraction
                train.fraction = 1,  # fraction of data for training
                n.minobsinnode = 10, # minimum number of obs for split
                keep.data=TRUE,      # store copy of input data in model
                cv.folds=5,          # number of cross validation folds
                verbose = TRUE,      # print progress
                n.cores = 7)         # use a single core

# cv results. gives best # trees. NDCG is probably inverse.
best.iter.ndcg <- gbm.perf(gbm.ndcg, method='cv')

#relative influence
summary(gbm.ndcg, n.trees=best.iter.ndcg, main='pairwise (ndcg)')

#make pred for test set
predictions <- predict(gbm.ndcg, test, best.iter.ndcg)

## does not work!
#N <- length(predictions)
#ndcg5.loss=sapply(1:length(predictions), FUN=function(i) {
#  gbm.loss(y=test$target, predictions[[i]], w=rep(1,N), offset=NA, dist=list(name='pairwise', metric="ndcg"),
#           baseline=0, group=test$srch_id, max.rank=38) })

## does work
id <- test$srch_id
label <- test$target

source("ndcg_function.R")
metric <- ndcg(label, id, predictions, 38)
