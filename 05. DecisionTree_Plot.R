#######Plot the trees-----------------------------------
# decision tree
model_decisionTree <- decision_tree() %>% # default parameters are: min_n = 20, tree_depth = 30, cost_complexity = 0.01
  set_mode("regression") %>%
  set_engine("rpart") %>%
  fit(formula = price ~ .,
      data = data4_all_ml_train)

class(model_decisionTree)

# common rpart.plot
rpart.plot::rpart.plot(model_decisionTree$fit, roundint=FALSE, main = "Decision Tree", cex.main = 1.5)

# fancy Rpart Plot
library(RColorBrewer)
library(rattle)
fancyRpartPlot(model_decisionTree$fit, main = "Decision Tree", cex.main = 1.5)


# Bagged Trees
model_bagged <- baguette::bag_tree() %>%
  set_mode("regression") %>%
  set_engine("rpart", times = 100) %>%
  fit(formula = price ~., data = data4_all_ml_train)

class(model_bagged) # Bagging tree is a predictive tool, so you will not see the picture of the actual tree 


# random Forest
model_randomForest <- rand_forest(mtry = 6, # there are 36 variables so squared 36 = 6
                                  trees = 500,
                                  min_n = 10) %>%
  set_mode("regression") %>%
  set_engine("randomForest") %>% # "ranger" or "randomForest" both ok
  fit(price ~ ., data = data4_all_ml_train)

class(model_randomForest)
typeof(model_randomForest$fit) # the model seems not the data


# XGBoost trees
library(xgboost)
library(caret)
library(dplyr)
library(DiagrammeR)
BoostedTree_plot <- xgb.plot.tree(model = final_model$preproc, trees = 1, plot_width = 1000, 
              plot_height = 1000, render = FALSE)
#export plot object to file
export_graph(BoostedTree_plot, "Boosted Tree.png", plot_width = 1000, 
             plot_height = 1000)

