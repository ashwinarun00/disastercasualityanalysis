# Install required packages (only uncomment if needed to install packages)
# install.packages("ggplot2")
# install.packages("reshape2")
# install.packages("gridExtra")
# install.packages("dplyr")
# install.packages("ggcorrplot")

# Load required libraries
library(readxl)
library(caret)
library(glmnet)
library(dplyr)
library(ggplot2)
library(reshape2)
library(gridExtra)
library(ggcorrplot)

# Load the data
df <- read_excel("model_data.xlsx")

# Columns to remove
columns_to_remove <- c("duration_binned", "Updated.Event.Name", "Country", "Continent", 
                       "Tropical.Status", "Hemisphere", "X")

# Remove the columns
df <- df %>% select(-one_of(columns_to_remove))

# Preprocessing
X <- df %>% select(-c(Deaths, Year...Month, Event, DataCards, Mapped.Event.Categories, 
                      Year, Month, duration_check, Losses..Local))
y <- df$Deaths

# Drop any columns with non-numeric data (if any)
non_numeric_cols <- X %>% select(where(is.character)) %>% names()
print(paste("Non-numeric columns:", toString(non_numeric_cols)))

# Convert non-numeric columns to factors if needed
X[non_numeric_cols] <- lapply(X[non_numeric_cols], as.factor)

# Split data into training and testing sets
set.seed(123)
train_indices <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[train_indices, ]
y_train <- y[train_indices]
X_test <- X[-train_indices, ]
y_test <- y[-train_indices]

# Ridge Regression
ridge_model <- cv.glmnet(as.matrix(X_train), y_train, alpha = 0, family = "gaussian")

# Predictions
predictions <- predict(ridge_model, s = "lambda.min", newx = as.matrix(X_test))

# Metrics
r2 <- cor(y_test, predictions)^2
mse <- mean((y_test - predictions)^2)
mae <- mean(abs(y_test - predictions))

cat("R²:", r2, "\nMSE:", mse, "\nMAE:", mae, "\n")

# Infrastructure Features and Correlation Heatmap
# Create a vector for the infrastructure features
infra_features <- c('Education', 'Agriculture', 'Health.sector', 'Water.supply',
                    'Communications', 'Transportation', 'Power.and.Energy')
df_infra <- df[, infra_features]

# Create the correlation matrix
corr_matrix <- cor(df_infra, use = "pairwise.complete.obs")

# Heatmap Plot
heatmap_plot <- ggcorrplot(
  corr_matrix,
  method = "square",
  hc.order = FALSE,
  lab = TRUE,
  lab_size = 3.5,  
  colors = c("cyan", "white", "#006d5b"), 
  title = "Infrastructure Interdependencies"
) +
  theme_minimal(base_size = 14) + 
  theme(
    plot.title = element_text(hjust = 0.5, size = 18, face = "bold"),  
    axis.text.x = element_text(angle = 45, hjust = 1, size = 12),  
    axis.text.y = element_text(size = 12)  
  )

# Print the heatmap
print(heatmap_plot)

# Define top 3 strongest relationships
top_pairs <- list(
  c('Power.and.Energy', 'Water.supply'),
  c('Power.and.Energy', 'Communications'),
  c('Education', 'Transportation')
)

# Prepare scatterplots with regression lines for top pairs
scatter_plots <- lapply(top_pairs, function(pair) {
  x_var <- pair[1]
  y_var <- pair[2]
  
  # Filter out missing values
  df_pair <- df %>% select(all_of(c(x_var, y_var))) %>% na.omit()
  
  # Fit linear model
  lm_model <- lm(as.formula(paste(y_var, "~", x_var)), data = df_pair)
  r_squared <- summary(lm_model)$r.squared
  
  # Create scatter plot with regression line
  ggplot(df_pair, aes_string(x = x_var, y = y_var)) +
    geom_point(alpha = 0.6, color = "#006d5b", size = 2) +
    geom_smooth(method = "lm", color = "blue", se = FALSE, linewidth = 1) + 
    labs(
      title = paste0("R² = ", round(r_squared, 3)),
      x = x_var,
      y = y_var
    ) +
    theme_minimal(base_size = 14) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 16, face = "bold"),  
      axis.text = element_text(size = 12), 
      axis.title = element_text(size = 14)
    )
})

# Arrange scatter plots into a grid
grid.arrange(grobs = scatter_plots, nrow = 1, top = "Top Infrastructure Relationships")
