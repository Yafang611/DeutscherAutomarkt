# To check data counts and decide the analysis scope
data2_summary <- data2 %>%
  group_by(make, offerType) %>%
  summarize(count = table(offerType))

summary_plot <- data2_summary %>%
  ggplot(aes(x = as.factor(make), y = as.numeric(count))) + 
  geom_col() +
  coord_flip()

data3_ml <- data2_summary %>%
  filter(count >= 500)

str(data2_summary)
str(data3_ml)

make_names <- levels(as.factor(data3_ml$make)) # to extract the elements of "make"

# to filter all data within the scope(used & 20 brands)
library(fastDummies)
data3_all_ml <- data2 %>% # a dataset with variable of "year"
  filter(offerType == "Used") %>%
  filter(make == make_names) %>%
  select(-last_col(offset=0:4, everything())) %>% #delete the previous dummy variables because used cars was decided for this analysis
  dummy_cols(select_columns = c("make", "fuel", "gear")) %>%
  select(where(is.numeric))

# rename the unrecognized column names
  colnames(data3_all_ml)[25] <- "fuel_Fuel" # rename the column because / is not be recognized
  colnames(data3_all_ml)[29] <- "fuel_Electric_Diesel"
  colnames(data3_all_ml)[30] <- "fuel_Electric_Gasoline"
  colnames(data3_all_ml)[37] <- "gear_SemiAutomatic"

sum(is.na(data3_all_ml))
View(data3_all_ml)
str(data3_all_ml)

# set a dataset without variable of "year"
library(corrplot)
data4_all_ml <- data3_all_ml %>%
  select(-year)

data4_all_ml %>%
  cor() %>%
  corrplot(mar = c(0, 0, 0, 0), tl.cex = 0.8, tl.col = "black") # adjust the parameters of corrplot

# after corrplot, figure out the single factor of price: mileage(-), hp(+), make_Audi(+), gear_Automatic(+), gear_Manual(-). Next to use ML with tree-based models (decion tree/bagging/random forest/Boosting) to check the feature importance when considering other variables 
  
########regression tree w/o cross-validation--------------------------------------------------
head(data4_all_ml)
nrow(data4_all_ml)
colnames(data4_all_ml) <- make.names(colnames(data4_all_ml)) # without this, the regression tree w/ cv will occur an error because of the invalid columns names.
str(data4_all_ml)

#construct the regression tree
library(tidymodels)
set.seed(1988)
data4_all_ml_split = initial_split(data4_all_ml, prop = 0.75)
data4_all_ml_train = training(data4_all_ml_split)
data4_all_ml_test = testing(data4_all_ml_split)
round(nrow(data4_all_ml_train) / nrow(data4_all_ml), 2) == 0.75 # verification
predictions_decisonTree <- decision_tree() %>% # default parameters are: min_n = 20, tree_depth = 30, cost_complexity = 0.01
  set_mode("regression") %>%
  set_engine("rpart") %>%
  fit(formula = price ~ .,
      data = data4_all_ml_train) %>%
  predict(data4_all_ml_test) %>%
  bind_cols(data4_all_ml_test)

mae_decisionTree <- mae(predictions_decisonTree, estimate = .pred, truth = price)
rmse_decisionTree <- rmse(predictions_decisonTree, estimate = .pred, truth = price) 
evaluations_decisionTree <- bind_rows(mae_decisionTree, rmse_decisionTree)
  
########regression tree w/ cross-validation on training data-----------------------------------
set.seed(1988)
data4_all_ml_train_folds <- vfold_cv(data4_all_ml_train, v = 10)

evaluation_decisonTree_cv <- decision_tree() %>% 
  set_engine("rpart") %>%
  set_mode("regression") %>%
  fit_resamples(price ~ .,
                 resamples = data4_all_ml_train_folds,
                 metrics = metric_set(mae, rmse)) %>%
  collect_metrics(summarize = TRUE) # 1. since resample was not building a model, you could not predict on the newdata after resampling. 2. if Summarize = FALSE, it shows mae and rmse results for each fold. (10 folds in total)

########to check bias&variance tradeoff--------------------------------------------------------
predictions_decisionTree_train <- decision_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart") %>%
  fit(formula = price ~ .,
      data = data4_all_ml_train) %>%
  predict(data4_all_ml_train) %>%
  bind_cols(data4_all_ml_train)

mae_decisionTree_train <- mae(predictions_decisionTree_train, estimate = .pred, truth = price)
rmse_decisionTree_train <- rmse(predictions_decisionTree_train, estimate = .pred, truth = price) 
evaluations_decisionTree_train <- bind_rows(mae_decisionTree_train, rmse_decisionTree_train)

evaluations_decisionTree_train # (1) train data
evaluations_decisionTree # (2) test data
evaluation_decisonTree_cv # (3) train data
# 1. when comparing (1) and (2), above results show that the mae and rmse perform better in-sample data (train data) than out-of-sample data(test data) => overfitting? need reduce the complexity? 
# 2. when comparing (1)-w/o cv and (3)-w/ cv, the only difference is w/ or w/o cross validation. The mae and rmse of w/ cv is larger than those of w/o cv, indicating that one way to reduce the variance from outliers is to average multiple estimates together(cross-validation)

############what can we do next? to reduce the tree_depth, cost_complexity or increase min_n of the tree models, which is not shown here------------------------------------------------------ 

########Hyperparameters and Ensemble Models-----------------------------------
#Step 1: Create placeholders: tune()
spec_untuned <- decision_tree(
  min_n = tune(),
  tree_depth = tune()
) %>%
  set_engine("rpart") %>%
  set_mode("regression")

#Step 2: Create a tuning grid: grid_regular()
tree_grid <- grid_regular(
  parameters(spec_untuned),
  levels = 5
)

#Step 3: Tune the grid: tune_grid() # tune on whole dataset rather than the trainset
set.seed(1988)
data4_all_ml_folds <- vfold_cv(data4_all_ml, v = 10)
tune_results <- tune_grid(
  spec_untuned,
  price ~ .,
  resamples = data4_all_ml_folds,
  grid = tree_grid,
  metrics = metric_set(mae, rmse)
)

autoplot(tune_results) # visualize the tuning results

#Step 4: Use the best parameters: finalize_model()
final_params <- select_best(tune_results) # select the best performing parameters
final_params

best_spec <- finalize_model(spec_untuned, final_params)

# Step 5: Pick the winner & predict on the test data
final_model <- fit(best_spec,
                   formula = price ~ .,
                   data = data4_all_ml)

predictions_decisionTree_Hyperparameters <-  predict(final_model, data4_all_ml) %>%
  bind_cols(data4_all_ml)
predictions_decisionTree_Hyperparameters

mae_decisionTree_Hyperparameters <- mae(predictions_decisionTree_Hyperparameters, estimate = .pred, truth = price)
rmse_decisionTree_Hyperparameters <- rmse(predictions_decisionTree_Hyperparameters, estimate = .pred, truth = price) 
evaluations_decisionTree_Hyperparameters <- bind_rows(mae_decisionTree_Hyperparameters, rmse_decisionTree_Hyperparameters)

# combine the evaluations by different models for easy comparison
evaluations <- bind_rows(evaluations_decisionTree_train,evaluations_decisionTree, evaluation_decisonTree_cv, evaluations_decisionTree_Hyperparameters,
          .id = "model")
evaluations_names <- c('decisionTree_train', 'decisionTree_test', 'decisonTree_cv_train', 'decisionTree_Hyperparameters_all')
for(i in 1:4){
evaluations$model[evaluations$model == i] <- evaluations_names[i]
} # rename the models
evaluations

#-------------------------Other Evaluations Methods: roc_auc()- can not be used in regression case because ROC is based on confused matrix (for classification)----------------------------------
#roc <- roc_curve(predictions_decisionTree_train,
#                 estimate = .pred,
#                 truth = as.factor(price))
#autoplot(roc)


# export into .csv or .excel on your computer
write.table(evaluations , file = "C:/Users/vicky/OneDrive/??????/Coding and Data Literacy/Kaggle/GermanyCar/evaluations.csv")
library(xlsx)
write.xlsx(evaluations , file = "C:/Users/vicky/OneDrive/??????/Coding and Data Literacy/Kaggle/GermanyCar/evaluations.xlsx")

