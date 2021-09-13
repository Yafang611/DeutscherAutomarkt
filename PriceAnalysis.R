lm(list = ls())
setwd("C:/Users/vicky/OneDrive/桌面/Coding and Data Literacy/Kaggle/GermanyCar")
data0 <- read.csv("autoscout24-germany-dataset.csv", header = TRUE, sep = ",")
plot(density(data0$price))

manufacturer <- levels(as.factor(data0$make)) # before obtaining the components of factor, converting into a factor is necessary

sum(is.na(data0))
apply(is.na(data0), 2, which) # to locate the .na position

library(dplyr)
data1 <- data0 %>%
  filter(!is.na(hp)) # filter out all missing data rows

sum(is.na(data1))

#dummy variables for offerType
library(fastDummies)
data2 <- data1 %>%
  dummy_cols(select_columns = "offerType")

View(data2)
df_offerType_all <- nrow(data2)

# create function for the next step of checking rows of value == 1 
fun <- function(x){
  length(which(x == 1))
}

# to select the last 5 columns for future summary analysis
n <- 5
data3 <- data2 %>% 
  select(last_col(offset=0:(n-1), everything())) # to select the last 5 columns
                            
df_offerType_single <- apply(data3,2,fun) # 2 means columns, 1 means rows
df_offerType <- append(df_offerType_single, df_offerType_all)
names(df_offerType)[6] <- c("offerType_all")

# cbind(df_offerType, df_offerType_all) - to add a column in a df rather than a element in a vector

# Virsualize the data # for each offerType
par(mar=c(5, 12, 2, 2)) # 对画布上下左右位置移动。数字代表移动的距离。
barplot(df_offerType[order(df_offerType, decreasing = FALSE)], col="#69b3a2", horiz=T, las = 1, cex.names = 1)

# Then found that used cars number is the most. ->analysis on used car could be more accurate? more data.


library(corrplot)
data4 <- data2 %>%
  dummy_cols(select_columns = c("fuel", "gear")) %>%# dummy fuel and gear without considering make and model
  select_if(is.numeric) %>%
  select(-year) %>%
  cor() %>%
  corrplot()

colnames(data4)[2] <- "price"
colnames(data4)[4] <- "offerType_Demonstration"
colnames(data4)[5] <- "offerType_EmployeesCar"
colnames(data4)[7] <- "offerType_PreRegistered"
colnames(data4)[9] <- "fuel_Fuel" # rename the column because / is not be recognized
colnames(data4)[13] <- "fuel_Electric_Diesel"
colnames(data4)[14] <- "fuel_Electric_Gasoline"
colnames(data4)[23] <- "gear_SemiAutomatic"


View(data4)

vars1 <- colnames(data4)
vars2 <- vars1[!vars1 %in% c("price")]
fmla <- paste("price", "~", paste(vars2, collapse = "+"))

str(data4) # check the data type

scaled <- scale(data4, center = TRUE, scale = TRUE)
#scaled <- data ## Here you can test what would happen if you used non-standardized data (need to # the line above then)

set.seed(1988)
rows <- sample(nrow(scaled)) # shuffle row indices: rows
shuffled_scaled <- scaled[rows, ] # randomly order data
split <- round(nrow(scaled) * 0.8)
train <- scaled[1:split, ]
test <- scaled[(split + 1):nrow(scaled), ]

sum(is.na(scaled))
apply(is.na(scaled), 2, which) # to locate the .na position

library(randomForest)
set.seed(1988)
rf <- randomForest(price ~ ., data = train, ntree = 1000, importance = TRUE)

varImpPlot(rf, main ='Feature importance')
test$price_rf <- predict(rf, newdata = test)
test$price_error_rf <- (test$price_rf - test$price)^2
mse_rf <- mean(test$price_error_rf)
