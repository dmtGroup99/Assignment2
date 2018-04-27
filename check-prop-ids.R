library(data.table)
library(ggplot2)
#train data exist out of about 4.9 million instances and 54 variables
data_train <- data.table::fread(input = "training_set_VU_DM_2014.csv", na.strings = c("NA", "NULL", "null"))
data_test <- data.table::fread(input = "test_set_VU_DM_2014.csv", na.strings = c("NA", "NULL", "null"))

prop_test <- unique(data_test$prop_id)
prop_train <- unique(data_train$prop_id)
length(prop_test[!prop_test %in% prop_train])
length(prop_test)

# 7773 prop_ids of test set are not in train set from total of 129438 prop ids of test set
# thus fruitfull to estimate popularity and rank position of prop_ids

ggplot(data_train,aes(position,fill=booking_bool))+
  geom_bar(stat="identity",position='dodge')

non_random <- subset(data_train, random_bool==0)
non_random$action <- non_random$booking_bool + non_random$click_bool
non_random <- subset(non_random, action != 0)

random <- subset(data_train, random_bool==1)
random$action <- random$booking_bool + random$click_bool
random <- subset(random, action != 0)

#for both plots it looks like rank position is an important factor
ggplot(aes(x = position, fill = as.factor(action) ) , data = non_random) + 
  geom_histogram()

ggplot(aes(x = position, fill = as.factor(action) ) , data = random) + 
  geom_histogram()


