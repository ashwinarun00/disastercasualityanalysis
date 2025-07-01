#load the packages
library(rpart)
library(rpart.plot)
library(caret)
library(ggpubr)
library(car)
library(randomForest)
library(xgboost)
library(readxl)
library(dplyr)
library(ggplot2)
library(factoextra)
library(dplyr)
library(FactoMineR)

#PRINCIPAL COMPONENT ANALYSIS

df <- read_excel("model_data_2.xlsx")

#Take numeric columns and scale them

numeric_df <- df %>% 
  select_if(is.numeric) %>% 
  select(-X, -DataCards)
scaled_df <- scale(as.matrix(numeric_df))

# Perform PCA
pca_result <- prcomp(scaled_df, center = TRUE, scale = TRUE)
summary(pca_result)
pca_result$x 
pca_result$rotation

# Visualize PCA results - 
#using elbow method and decide the no of PCs - where the explained variance starts leveling off
fviz_eig(pca_result, barfill = "#5FAFAF", barcolor = "#5FAFAF") # Scree plot

#Plot cumulative variance contribution
explained_variance <- (pca_result$sdev^2) / sum(pca_result$sdev^2)
cumulative_variance <- cumsum(explained_variance)

pc_number <- seq_along(explained_variance)
variance_df <- data.frame(
  PC = pc_number,
  ExplainedVariance = explained_variance,
  CumulativeVariance = cumulative_variance
)

ggplot(variance_df, aes(x = PC)) +
  geom_line(aes(y = CumulativeVariance), color = "orange", size = 1) +
  geom_point(aes(y = CumulativeVariance), color = "orange", size = 2) +
  labs(
    title = "Cumulative Variance Cont",
    x = "Principal Component (PC)",
    y = "Variance Explained (%)"
  ) +
  theme_minimal()


#POST PCA CALCULATION
#Transform data
n_components <- 3

X_pca <- as.data.frame(pca_result$x[, 1:n_components])
pca_data <- cbind(X_pca, y = df$Updated.Event.Name)
write.csv(pca_data, "pca_data_eventcat_upd.csv", row.names=FALSE)

X_pca <- as.data.frame(pca_result$x[, 1:n_components])
pca_data <- cbind(X_pca, y = df$Mapped.Event.Categories)
write.csv(pca_data, "pca_data_eventcat_map.csv", row.names=FALSE)

X_pca <- as.data.frame(pca_result$x[, 1:n_components])
pca_data <- cbind(X_pca, y = df$Continent)
write.csv(pca_data, "pca_data_continent.csv", row.names=FALSE)

X_pca <- as.data.frame(pca_result$x[, 1:n_components])
pca_data <- cbind(X_pca, y = df$country_group)
write.csv(pca_data, "pca_data_countrygrp.csv", row.names=FALSE)

#CLASSIFICATION

#Reading file
df <- read_excel("model_data_2.xlsx")
df <- df %>% select(-X)
df$Updated.Event.Name <- as.factor(df$Updated.Event.Name)
df$Mapped.Event.Categories <- as.factor(df$Mapped.Event.Categories)
df$Continent <- as.factor(df$Continent)
df$country_group <- as.factor(df$country_group)

#PERFORMING FEATURE SELECTION

#removing columns via low variance threshold
#variances <- apply(df[, sapply(df, is.numeric)], 2, var)
#threshold <- 0.05
#df_filtered <- df[, variances > threshold]

#using rf importance scores to sort important columns
rf_model <- randomForest(Mapped.Event.Categories ~ ., data = df, importance = TRUE, ntree=200)
importance_scores <- importance(rf_model)
selected_vars <- rownames(importance_scores)[order(importance_scores[, 1], decreasing = TRUE)[1:30]]

selected_importance_scores <- importance_scores[selected_vars, 1]

#creating a data frame with importance scores and selected variables
importance_table <- data.frame(Variable = selected_vars,Importance = selected_importance_scores)
print(importance_table)


#Modelling

model_train <- function(df) {
  
  # Set seed for reproducibility
  set.seed(999)
  
  # Split data into training and testing sets
  train_index <- createDataPartition(df$y, p = 0.8, list = FALSE)
  train_data <- df[train_index, ]
  test_data <- df[-train_index, ]
  
  #### CART ####
  print("CART MODEL RESULTS")
  cart_model <- rpart(y ~ ., data = train_data, method = "class", parms = list(split = 'gain'))
  print(summary(cart_model))
  
  # Visualize the decision tree
  rpart.plot(cart_model, type = 4, extra = 102, under = TRUE, nn = TRUE)
  
  # Predictions and evaluation for CART
  cart_predictions <- predict(cart_model, newdata = test_data, type = "class")
  cart_confusion_matrix <- confusionMatrix(cart_predictions, test_data$y)
  print(table(Predicted = cart_predictions, Actual = test_data$y))
  print(cart_confusion_matrix)
  
  #### RANDOM FOREST ####
  print("RF MODEL RESULTS")
  rf_model <- randomForest(y ~ ., data = train_data, ntree = 500, mtry = 3, importance = TRUE)
  plot(rf_model, main = "Error Plot")
  print(rf_model$importance)
  
  # Predictions and evaluation for Random Forest
  rf_predictions <- predict(rf_model, newdata = test_data)
  rf_confusion_matrix <- confusionMatrix(rf_predictions, test_data$y)
  print(table(Predicted = rf_predictions, Actual = test_data$y))
  print(rf_confusion_matrix)
}

# MANUAL MODELLING
##cont var - model
df_filtered <- df %>% select(CountryGuatemala, Tropical.StatusYes, population, Losses..Local, gdp_per_capita, Continent)
df_filtered <- df_filtered %>% rename(y = Continent)
df_filtered$y <- as.factor(df_filtered$y)
df_cont <- data.frame(df_filtered)
model_train(df_cont)


##upd var - model
df_filtered <- df %>% select(Updated.Event.NameACCIDENT, Duration..d._log, population, Houses.Damaged_norm, gdp_per_capita, Updated.Event.Name)
df_filtered <- df_filtered %>% rename(y = Updated.Event.Name)
df_filtered$y <- as.factor(df_filtered$y)
df_upd <- data.frame(df_filtered)
model_train(df_upd)

##map var - model
df_filtered <- df %>% select(Updated.Event.NameANIMAL, duration_in_yrs, Houses.Damaged, population, gdp_per_capita, Mapped.Event.Categories)
df_filtered <- df_filtered %>% rename(y = Mapped.Event.Categories)
df_filtered$y <- as.factor(df_filtered$y)
df_map <- data.frame(df_filtered)
model_train(df_map)

##country grp var - model
df_filtered <- df %>% select(population, gdp_per_capita, Tropical.StatusNo, CountryMexico, Losses..Local, ContinentAsia, country_group)
df_filtered <- df_filtered %>% rename(y = country_group)
df_filtered$y <- as.factor(df_filtered$y)
df_ctygrp <- data.frame(df_filtered)
model_train(df_ctygrp)


## 2 - USING PCs 

##a) Continents
df <- read.csv("pca_data_continent.csv")
df$y <- as.factor(df$y)

model_train(df)

#b) Updated Event Name
df <- read.csv("pca_data_eventcat_upd.csv")
df$y <- as.factor(df$y)

model_train(df)

#c) Mapped Event Categories

df <- read.csv("pca_data_eventcat_map.csv")
df$y <- as.factor(df$y)

model_train(df)

#d) Country Group
df <- read.csv("pca_data_countrygrp.csv")
df$y <- as.factor(df$y)

model_train(df)
