

# LOAD LIBRARIES

library(dplyr)
library(tidyr)
library(caret)
library(randomForest)
library(ggplot2)

set.seed(42)


# IMPORT DATA

data_path <- "C:/Users/Azzah Syafaruddin/OneDrive - Universiti Kebangsaan Malaysia/FYP2/updated_dataset.csv"
data_cars <- read.csv(data_path)


# HANDLE MISSING VALUES

data_cars$car_variant[is.na(data_cars$car_variant)] <- "no variant"
data_cars$used_dealer_company_id[is.na(data_cars$used_dealer_company_id)] <- 999999
data_cars <- na.omit(data_cars)

# TARGET MEAN ENCODING (BRAND, MODEL, VARIANT)

carbrand_transform <- data_cars %>% 
  group_by(car_brand) %>%
  summarise(mean_reserveprice = mean(reserveprice)) %>%
  mutate(carbrand_rank = dense_rank(mean_reserveprice))

carmodel_transform <- data_cars %>% 
  group_by(car_model) %>%
  summarise(mean_reserveprice = mean(reserveprice)) %>%
  mutate(carmodel_rank = dense_rank(mean_reserveprice))

carvariant_transform <- data_cars %>% 
  group_by(car_variant) %>%
  summarise(mean_reserveprice = mean(reserveprice)) %>%
  mutate(carvariant_rank = dense_rank(mean_reserveprice))

new_df <- data_cars %>% 
  left_join(carbrand_transform %>% select(car_brand, carbrand_rank), by = "car_brand") %>%
  left_join(carmodel_transform %>% select(car_model, carmodel_rank), by = "car_model") %>%
  left_join(carvariant_transform %>% select(car_variant, carvariant_rank), by = "car_variant") %>%
  select(-car_brand, -car_model, -car_variant) %>%
  rename(
    car_brand   = carbrand_rank,
    car_model   = carmodel_rank,
    car_variant = carvariant_rank
  )

# FREQUENCY ENCODING (ID COLUMNS)

new_df$lead_id <- as.numeric(table(data_cars$lead_id)[as.character(new_df$lead_id)])
new_df$marketplace_id <- as.numeric(table(data_cars$marketplace_id)[as.character(new_df$marketplace_id)])
new_df$marketplace_car_id <- as.numeric(table(data_cars$marketplace_car_id)[as.character(new_df$marketplace_car_id)])
new_df$used_dealer_company_id <- as.numeric(table(data_cars$used_dealer_company_id)[as.character(new_df$used_dealer_company_id)])
new_df$dealer_id <- as.numeric(table(data_cars$dealer_id)[as.character(new_df$dealer_id)])

# ENCODE TRANSMISSION

new_df$car_transmission <- as.integer(data_cars$car_transmission == "Manual")

# TRAIN–TEST SPLIT

train_index <- createDataPartition(new_df$reserveprice, p = 0.8, list = FALSE)
train_data <- new_df[train_index, ]
test_data  <- new_df[-train_index, ]

# TRAIN RANDOM FOREST MODEL

rf_model <- randomForest(
  reserveprice ~ .,
  data = train_data,
  ntree = 500,
  mtry = floor(ncol(train_data) / 3),
  nodesize = 10,
  importance = TRUE
)

# PREDICTION

rf_pred <- predict(rf_model, newdata = test_data)
y_true <- test_data$reserveprice

# PERFORMANCE METRICS

MSE  <- mean((y_true - rf_pred)^2)
RMSE <- sqrt(MSE)
MAPE <- mean(abs((y_true - rf_pred) / y_true)) * 100
R2   <- 1 - sum((y_true - rf_pred)^2) / sum((y_true - mean(y_true))^2)

performance_rf <- data.frame(
  MSE = MSE,
  RMSE = RMSE,
  MAPE = MAPE,
  R2 = R2
)

performance_rf

# ACTUAL vs PREDICTED PLOT 

df_plot <- data.frame(
  Actual = y_true,
  Predicted = rf_pred
)

ggplot(df_plot, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linetype = "dashed") +
  labs(
    title = "Harga Rizab Diramal vs Harga Rizab Sebenar (Hutan Rawak)",
    x = "Harga Rizab Sebenar",
    y = "Harga Rizab Diramal"
  ) +
  theme_minimal()

library(dplyr)
library(caret)
library(xgboost)
library(ggplot2)

set.seed(42)

# Import data
data_path <- "C:/Users/Azzah Syafaruddin/OneDrive - Universiti Kebangsaan Malaysia/FYP2/updated_dataset.csv"
data_cars <- read.csv(data_path)

# Basic data cleaning
data_cars$car_variant[is.na(data_cars$car_variant)] <- "no variant"
data_cars$used_dealer_company_id[is.na(data_cars$used_dealer_company_id)] <- 999999
data_cars <- na.omit(data_cars)

# Feature engineering
data_cars$car_age <- 2024 - data_cars$car_year

# Label encoding 
cat_cols <- names(data_cars)[sapply(data_cars, is.character)]
cat_cols <- setdiff(cat_cols, "reserveprice")

for (col in cat_cols) {
  data_cars[[col]] <- as.integer(factor(data_cars[[col]]))
}

# Train–test split
train_index <- createDataPartition(
  data_cars$reserveprice,
  p = 0.8,
  list = FALSE
)

train_data <- data_cars[train_index, ]
test_data  <- data_cars[-train_index, ]

# Prepare matrix for XGBoost
X_train <- as.matrix(train_data %>% select(-reserveprice))
y_train <- train_data$reserveprice

X_test  <- as.matrix(test_data %>% select(-reserveprice))
y_test  <- test_data$reserveprice

dtrain <- xgb.DMatrix(data = X_train, label = y_train)
dtest  <- xgb.DMatrix(data = X_test,  label = y_test)

# Train XGBoost model
params <- list(
  objective = "reg:squarederror",
  eta = 0.05,
  max_depth = 6,
  subsample = 0.7,
  colsample_bytree = 0.8
)

xgb_model <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 300,
  verbose = 1
)

# Prediction
xgb_pred <- predict(xgb_model, dtest)

# Performance metrics
MSE  <- mean((y_test - xgb_pred)^2)
RMSE <- sqrt(MSE)
MAPE <- mean(abs((y_test - xgb_pred) / y_test)) * 100
R2   <- 1 - sum((y_test - xgb_pred)^2) / sum((y_test - mean(y_test))^2)

performance_xgb <- data.frame(
  MSE = MSE,
  RMSE = RMSE,
  MAPE = MAPE,
  R2 = R2
)

performance_xgb

# Actual vs Predicted plot 
df_plot <- data.frame(
  Actual = y_test,
  Predicted = xgb_pred
)

ggplot(df_plot, aes(x = Actual, y = Predicted)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_abline(intercept = 0, slope = 1,
              color = "red", linetype = "dashed") +
  labs(
    title = "Harga Rizab Diramal vs Harga Rizab Sebenar (XGBoost)",
    x = "Harga Rizab Sebenar",
    y = "Harga Rizab Diramal"
  ) +
  theme_minimal()
                                                                  