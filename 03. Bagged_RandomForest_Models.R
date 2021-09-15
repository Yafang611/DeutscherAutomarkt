######## Bagged Trees(Bootstrap Aggregation)--------------------------------------------------
predictions_bagged_train <- baguette::bag_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart", times = 100) %>%
  fit(formula = price ~., data = data4_all_ml_train) %>%
  predict(new_data = data4_all_ml_train) %>%
  bind_cols(data4_all_ml_train)

mae_bagged_train <- mae(predictions_bagged_train, estimate = .pred, truth = price)
rmse_bagged_train <- rmse(predictions_bagged_train, estimate = .pred, truth = price) 
evaluations_bagged_train <- bind_rows(mae_bagged_train, rmse_bagged_train)

predictions_bagged_test <- baguette::bag_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart", times = 100) %>%
  fit(formula = price ~., data = data4_all_ml_train) %>% # fit on train dataset but predict on testset
  predict(new_data = data4_all_ml_test) %>%
  bind_cols(data4_all_ml_test)

mae_bagged_test <- mae(predictions_bagged_test, estimate = .pred, truth = price)
rmse_bagged_test <- rmse(predictions_bagged_test, estimate = .pred, truth = price) 
evaluations_bagged_test <- bind_rows(mae_bagged_test, rmse_bagged_test)

# estimate mae & rmse by using cross-validation
spec_bagged_cv <- baguette::bag_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart", times = 100)

set.seed(1988)
results_bagged_cv_train <- fit_resamples(spec_bagged_cv,
  price ~ .,
  resamples = vfold_cv(data = data4_all_ml_train, v = 10),
  metrics = metric_set(mae, rmse))

evaluations_bagged_cv_train <- collect_metrics(results_bagged_cv_train)

######## Random Forest------------------------------------------------------------
# predict on train set
library(ranger)
predictions_randomForest_train <- rand_forest(mtry = 6, # there are 36 variables so squared 36 = 6
                                              trees = 500,
                                              min_n = 10) %>%
  set_mode("regression") %>%
  set_engine("ranger") %>% # "ranger" or "randomForest" both ok
  fit(price ~ ., data = data4_all_ml_train) %>%
  predict(new_data = data4_all_ml_train) %>%
  bind_cols(data4_all_ml_train)

mae_randomForest_train <- mae(predictions_randomForest_train, estimate = .pred, truth = price)
rmse_randomForest_train <- rmse(predictions_randomForest_train, estimate = .pred, truth = price) 
evaluations_randomForest_train <- bind_rows(mae_randomForest_train, rmse_randomForest_train)

# variable Importance
VariableImportance_randomForest <- rand_forest(mtry = 6, # there are 36 variables so squared 36 = 6
            trees = 500,
            min_n = 10) %>%
  set_mode("regression") %>%
  set_engine("ranger", importance = "impurity") %>%
  fit(price ~ ., data = data4_all_ml_train) %>%
  vip::vip() # pass the results to vip() function, which calculates the variable importance for our predictors.
###################---------The results seem similar to correlation plot(coincidence or ?).--------

# prediction on test set 
predictions_randomForest_test <- rand_forest(mtry = 6, # there are 36 variables so squared 36 = 6
                                              trees = 500,
                                              min_n = 10) %>%
  set_mode("regression") %>%
  set_engine("ranger") %>% # "ranger" or "randomForest" both ok
  fit(price ~ ., data = data4_all_ml_train) %>% # model built on train set but fit on test set
  predict(new_data = data4_all_ml_test) %>%
  bind_cols(data4_all_ml_test)

mae_randomForest_test <- mae(predictions_randomForest_test, estimate = .pred, truth = price)
rmse_randomForest_test <- rmse(predictions_randomForest_test, estimate = .pred, truth = price) 
evaluations_randomForest_test <- bind_rows(mae_randomForest_test, rmse_randomForest_test)


######## Combine Evaluations ----------------------------------
evaluations <- bind_rows(evaluations_decisionTree_train,evaluations_decisionTree, evaluation_decisonTree_cv, evaluations_decisionTree_Hyperparameters, evaluations_bagged_train, evaluations_bagged_test, evaluations_bagged_cv_train,evaluations_randomForest_train,evaluations_randomForest_test,.id = "model")
evaluations_names <- c('decisionTree_train', 'decisionTree_test', 'decisonTree_cv_train', 'decisionTree_Hyperparameters_all', 'bagged_train', 'bagged_test', 'bagged_cv_train', 'randomForest_train', 'randomForest_test' )
for(i in 1:9){
  evaluations$model[evaluations$model == i] <- evaluations_names[i]
} # rename the models

evaluations


# export into .csv or .excel on your computer
write.table(evaluations , file = "C:/Users/vicky/OneDrive/??????/Coding and Data Literacy/Kaggle/GermanyCar/evaluations.csv")
library(xlsx)
write.xlsx(evaluations , file = "C:/Users/vicky/OneDrive/??????/Coding and Data Literacy/Kaggle/GermanyCar/evaluations.xlsx")