####################Boosted Trees---------------------------------------------
library(xgboost)
predictions_boostTrees_train <- boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost") %>%
  fit(price ~ ., 
      data = data4_all_ml_train) %>%
  predict(new_data = data4_all_ml_train) %>%
  bind_cols(data4_all_ml_train)

mae_boostTrees_train <- mae(predictions_boostTrees_train, estimate = .pred, truth = price)
rmse_boostTrees_train <- rmse(predictions_boostTrees_train, estimate = .pred, truth = price) 
evaluations_boostTrees_train <- bind_rows(mae_boostTrees_train, rmse_boostTrees_train)

# on test dataset
predictions_boostTrees_test <- boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost") %>%
  fit(price ~ ., 
      data = data4_all_ml_train) %>%
  predict(new_data = data4_all_ml_test) %>%
  bind_cols(data4_all_ml_test)

mae_boostTrees_test <- mae(predictions_boostTrees_test, estimate = .pred, truth = price)
rmse_boostTrees_test <- rmse(predictions_boostTrees_test, estimate = .pred, truth = price) 
evaluations_boostTrees_test <- bind_rows(mae_boostTrees_test, rmse_boostTrees_test)

# test on train dataset w/ cross validation
set.seed(1988)
evaluations_boostTrees_cv_train <- boost_tree() %>%
  set_mode("regression") %>%
  set_engine("xgboost") %>%
  fit_resamples(price ~ ., 
                resamples = data4_all_ml_train_folds,
                metrics = metric_set(mae, rmse)) %>%
  collect_metrics()
  
# Optimize the boosted ensemble
# Step 1: create the specification with placeholders
boost_spec <- boost_tree(
  trees = 500,
  learn_rate = tune(),
  tree_depth = tune(),
  sample_size = tune()
  ) %>%
  set_mode("regression") %>%
  set_engine("xgboost")

#Step 2: create the tuning grid
tunegrid_boost <- grid_regular(
  parameters(boost_spec),
  levels = 5)

# Step 3: The tuning & visualize the result
tune_results <- tune_grid(
  boost_spec,
  price ~ .,
  resamples = data4_all_ml_train_folds,
  grid = tunegrid_boost,
  metrics = metric_set(mae, rmse)
)

autoplot(tune_results)

# Step 4: Finalize the model
best_params <- select_best(tune_results)
best_params

best_spec <- finalize_model(boost_spec,
                              best_params)

# Step 5: Train the final model
final_model <- fit(best_spec, price ~ ., data = data4_all_ml_train)
 
# (1) predictions and evaluations on all dataset  
predictions_boostTrees_tuning_all <- predict(final_model, new_data = data4_all_ml) %>%
  bind_cols(data = data4_all_ml)

mae_boostTrees_tuning_all <- mae(predictions_boostTrees_tuning_all, estimate = .pred, truth = price)
rmse_boostTrees_tuning_all <- rmse(predictions_boostTrees_tuning_all, estimate = .pred, truth = price) 
evaluations_boostTrees_tuning_all <- bind_rows(mae_boostTrees_tuning_all, rmse_boostTrees_tuning_all)

# (2) predictions and evaluations on train dataset  
predictions_boostTrees_tuning_train <- predict(final_model, new_data = data4_all_ml_train) %>%
  bind_cols(data = data4_all_ml_train)

mae_boostTrees_tuning_train <- mae(predictions_boostTrees_tuning_train, estimate = .pred, truth = price)
rmse_boostTrees_tuning_train <- rmse(predictions_boostTrees_tuning_train, estimate = .pred, truth = price) 
evaluations_boostTrees_tuning_train <- bind_rows(mae_boostTrees_tuning_train, rmse_boostTrees_tuning_train)

# (3) predictions and evaluations on test dataset  
predictions_boostTrees_tuning_test <- predict(final_model, new_data = data4_all_ml_test) %>%
  bind_cols(data = data4_all_ml_test)

mae_boostTrees_tuning_test <- mae(predictions_boostTrees_tuning_test, estimate = .pred, truth = price)
rmse_boostTrees_tuning_test <- rmse(predictions_boostTrees_tuning_test, estimate = .pred, truth = price) 
evaluations_boostTrees_tuning_test <- bind_rows(mae_boostTrees_tuning_test, rmse_boostTrees_tuning_test)
  
######## Combine Evaluations ----------------------------------
evaluations <- bind_rows(evaluations_decisionTree_train,evaluations_decisionTree, evaluation_decisonTree_cv, 
                         evaluations_decisionTree_Hyperparameters, 
                         evaluations_bagged_train, evaluations_bagged_test, evaluations_bagged_cv_train,
                         evaluations_randomForest_train,evaluations_randomForest_test,
                         evaluations_boostTrees_train, evaluations_boostTrees_test, evaluations_boostTrees_cv_train, 
                         evaluations_boostTrees_tuning_all, evaluations_boostTrees_tuning_train, evaluations_boostTrees_tuning_test, 
                         .id = "model")
evaluations_names <- c('decisionTree_train', 'decisionTree_test', 'decisonTree_cv_train', 
                       'decisionTree_Hyperparameters_all', 
                       'bagged_train', 'bagged_test', 'bagged_cv_train', 
                       'randomForest_train', 'randomForest_test',
                       'boostTrees_train', 'boostTrees_test', 'boostTrees_train_cv',
                       'boostTrees_tuning_all', 'boostTrees_tuning_train', 'boostTrees_tuning_test')
for(i in 1:15){
  evaluations$model[evaluations$model == i] <- evaluations_names[i]
} # rename the models

evaluations <- evaluations %>%
  mutate(result = if_else(is.na(.estimate), mean, .estimate))

View(evaluations)
 
library(ggplot2)
ggplot(evaluations, aes(x = result, y = model, fill = .metric)) +
  geom_col(position = "dodge") +
  labs(title = "Evaluations Summary Based on Different Models") +
  theme(plot.title = element_text(hjust = 0.5))
#  geom_text(aes(label = round(result,0), size = 3, hjust = 0, vjust = 0))


  

# export into .csv or .excel on your computer
write.table(evaluations , file = "C:/Users/vicky/OneDrive/??????/Coding and Data Literacy/Kaggle/GermanyCar/evaluations.csv")
library(xlsx)
write.xlsx(evaluations , file = "C:/Users/vicky/OneDrive/??????/Coding and Data Literacy/Kaggle/GermanyCar/evaluations.xlsx")



