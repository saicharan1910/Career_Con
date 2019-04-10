setwd('~/Downloads/career-con-2019/')

library(dplyr)
library(ggplot2)
library(ggthemes)
library(tidyr)
library(zoo)
library(randomForest)
library(caret)
library(xgboost)
library(corrplot)
library(splitstackshape)


x_train <- fread('X_train.csv')
y_train <- fread('y_train.csv')
x_test <- fread('X_test.csv')

cols <- c('orientation_X', 'orientation_Y', 'orientation_Z', 'orientation_W', 
          'angular_velocity_X', 'angular_velocity_Y', 'angular_velocity_Z', 
          'linear_acceleration_X', 'linear_acceleration_Y', 'linear_acceleration_Z')

# Exploration of the x variables
# Histogram of all the columns
x_train %>% 
  select(cols) %>% 
  gather() %>% 
  ggplot(aes(x = value)) + 
  geom_density(color="darkblue", fill="lightblue") + 
  facet_wrap(~key, scales = 'free') + 
  theme_fivethirtyeight() + 
  ggtitle('Distribution of all the x variables')
# angular_velocity & linear_acceleration seems to be normally distributed. orientation seems to be b/w -1 and 1 (tanh)

# Distribution of y variable
y_train %>% 
  group_by(surface) %>% 
  summarise(count = n()) %>% 
  arrange(count) %>% 
  mutate(surface = factor(surface, surface)) %>% 
  ggplot(aes(x = surface, y = count)) + 
  geom_bar(stat = 'identity', color="darkblue", fill="lightblue") + 
  geom_text(aes(label = count)) + 
  coord_flip()

# Plots for a single example 
x_train %>% 
  filter(series_id == 2863) %>% 
  select(c(cols, measurement_number)) %>% 
  gather(activity, value, -measurement_number) %>% 
  ggplot(aes(x = measurement_number, y = value)) + 
  geom_line() + 
  facet_wrap(~activity, scales = 'free') + 
  theme_fivethirtyeight() + 
  ggtitle('Time series plot for a single example')  

# Basic understanding of the features 
# Orientation x,y,z,w seems to be constantly decreasing or inceasing. Reason --> Position. it might moving in a direction
# Linear acceleration & angular velocity could help us in identifying in the surface. Hypothesis --> Rough surfaces might have lesser acceleration in general.

# Correlation Plot
M <- cor(x_train %>% select(cols))
corrplot(M, method = 'number', bg = 'lightblue')

# Compare test and train distribution
combined <- x_train %>% 
  select(cols) %>% 
  gather(entity, value) %>% 
  mutate(type = 'train') %>% 
  rbind(x_test %>% select(cols) %>% gather(entity, value) %>% mutate(type = 'test'))

ggplot(combined, aes(x = value, group = type, color = type)) + 
  geom_density() + 
  facet_wrap(~entity, scales = 'free') + 
  theme_fivethirtyeight() + 
  ggtitle('Test vs train distribution')  


#############################################################
## Ideas/features in mind
# 1. min
# 2. max
# 3. avg 
# 4. std 
# 5. rate of change final & abs(rate of change)
# 6. rate of change at multiple points & abs
# 7. moving averages
# 8. final - initial & abs(final - initial)
# 9. Regression coeffs 
# 10. diff

source('features_new.R')
features_df <- generate_features(x_train) %>% 
  inner_join(y_train) %>% 
  mutate(surface = factor(surface))

submission_features_df <- generate_features(x_test) 

# 5 fold cv
folds <- createFolds(features_df$surface, k = 5, list = FALSE)
features_df$fold <- folds
nfolds <- 5

validation_df <- data.frame()
submission_df <- data.frame()

for (i in 1:nfolds){
  train_set <- features_df %>% 
    filter(fold != i) %>% 
    as.data.frame() %>% 
    select(-fold, -series_id, -group_id)
  
  validation_set <- features_df %>% 
    filter(fold == i) %>% 
    as.data.frame() %>% 
    select(-fold)
  
  rf <- randomForest(surface ~ ., train_set, ntrees = 500)
  
  validation_set$predictions <- predict(rf, validation_set, type = "class")
  confusionMatrix(validation_set$predictions, validation_set$surface)
  
  # n fold Accuracy
  print(paste(i, 'Fold accuracy :' , round(mean(validation_set$predictions == validation_set$surface),2)))
  
  validation_df <- rbind(validation_df, validation_set %>% select(series_id, surface, predictions))
  
  # submission predictions 
  submission_df <- rbind(submission_df, 
                         data.frame(series_id = submission_features_df$series_id, 
                                    predict(rf, submission_features_df, type = 'prob')))

}

# Overall Accuracy 
print(paste('Overall Accuracy : ', round(mean(validation_df$predictions== validation_df$surface),2)))

# Submission 
submissions <- submission_df %>% 
  gather(surface, prob, -series_id) %>% 
  group_by(series_id, surface) %>% 
  summarise(mean_prob = mean(prob)) %>% 
  group_by(series_id) %>% 
  dplyr::slice(which.max(mean_prob)) %>% 
  ungroup() %>% 
  select(series_id, surface)

write.csv(submissions, 'submissions_v5.csv', row.names = F)


