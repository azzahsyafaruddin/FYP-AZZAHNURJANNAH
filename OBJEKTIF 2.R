library(dplyr)
library(ranger)

set.seed(123)

# Load data
file_path <- "C:/Users/Azzah Syafaruddin/OneDrive - Universiti Kebangsaan Malaysia/FYP2/updated_dataset.csv"
data_cars <- read.csv(file_path)

# Filter columns, handle NAs, and convert characters to factors
rf_data <- data_cars %>%
  select(reserveprice, car_brand, car_model, car_variant, car_engine,
         car_year, car_transmission, car_type, car_age, brand_price_label) %>%
  na.omit() %>%
  mutate(across(where(is.character), as.factor))

# Run Random Forest (Permutation Importance)
rf_perm <- ranger(reserveprice ~ ., data = rf_data, importance = "permutation",
                  num.trees = 500, seed = 123)

# Run Random Forest (Impurity Importance)
rf_imp <- ranger(reserveprice ~ ., data = rf_data, importance = "impurity",
                 num.trees = 500, seed = 123)

# Combine results for plotting
imp_df <- data.frame(
  variable = names(rf_perm$variable.importance),
  IncMSE = rf_perm$variable.importance,
  IncNodePurity = rf_imp$variable.importance
)

# Sort based on IncMSE
imp_df <- imp_df[order(imp_df$IncMSE), ]

# Setup plot layout
par(mfrow = c(1, 2), mar = c(5, 8, 4, 2), oma = c(0, 0, 3, 0))

# Plot 1: %IncMSE
dotchart(imp_df$IncMSE, labels = imp_df$variable,
         xlab = "%IncMSE", main = "%IncMSE", pch = 19)
text(x = imp_df$IncMSE, y = seq_along(imp_df$IncMSE),
     labels = formatC(imp_df$IncMSE, format = "e", digits = 2),
     pos = 4, cex = 0.7)

# Plot 2: IncNodePurity
dotchart(imp_df$IncNodePurity, labels = imp_df$variable,
         xlab = "IncNodePurity", main = "IncNodePurity", pch = 19)
text(x = imp_df$IncNodePurity, y = seq_along(imp_df$IncNodePurity),
     labels = formatC(imp_df$IncNodePurity, format = "e", digits = 2),
     pos = 4, cex = 0.7)

# Main title
mtext("Random Forest Feature Importance", outer = TRUE, line = 1, cex = 1.2, font = 2)

library(dplyr)
library(xgboost)
library(ggplot2)

set.seed(123)

# Load dataset
file_path <- "C:/Users/Azzah Syafaruddin/OneDrive - Universiti Kebangsaan Malaysia/FYP2/updated_dataset.csv"
data_cars <- read.csv(file_path)

# Select relevant variables and remove missing values
xgb_data <- data_cars %>%
  select(reserveprice, car_brand, car_model, car_variant, car_engine,
         car_year, car_transmission, car_type, car_age, brand_price_label) %>%
  na.omit()

# Feature Engineering: Convert categorical columns to numeric
# XGBoost requires a numeric matrix, so we apply label encoding
features <- setdiff(names(xgb_data), "reserveprice")

for (col in features) {
  if (is.character(xgb_data[[col]]) || is.factor(xgb_data[[col]])) {
    xgb_data[[col]] <- as.numeric(as.factor(xgb_data[[col]]))
  }
}

# Create XGBoost matrix
dtrain <- xgb.DMatrix(
  data = as.matrix(xgb_data[, features]),
  label = xgb_data$reserveprice
)

# Train the model
model_xgb <- xgboost(
  data = dtrain,
  nrounds = 100,
  objective = "reg:squarederror",
  verbose = 0
)

# Calculate Feature Importance (Gain)
importance_matrix <- xgb.importance(feature_names = features, model = model_xgb)

# Prepare data for plotting
# Using head(10) instead of slice() to avoid package conflicts
plot_data <- importance_matrix %>%
  as.data.frame() %>%
  arrange(desc(Gain)) %>%
  head(10)

# Print top features to console
print(plot_data)

# Generate Feature Importance Plot (Clean Style)
ggplot(plot_data, aes(x = reorder(Feature, Gain), y = Gain)) +
  geom_col(fill = "steelblue", width = 0.7) +
  coord_flip() +
  theme_minimal() +
  labs(
    title = "Feature Importance",
    x = NULL,
    y = "Gain"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.text.y = element_text(size = 11, color = "black"),
    panel.grid.major.y = element_blank()
  )
