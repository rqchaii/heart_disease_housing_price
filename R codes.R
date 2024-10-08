setwd("~/Downloads")

# UNSUPERVISED LEARNING
library(corrplot)
library(tidyverse)
library(lubridate)
library(plotly)
library(cluster)
library(factoextra)
library(plyr)
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyr)
library(patchwork)
library(ISLR)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dendextend)

# DATA 

data <- read.csv("Country-data.csv")

glimpse(data)
summary(data)
any(is.na(data))

#numeric_data
numeric_data <- data[, sapply(data, is.numeric)]
summary(numeric_data)

## Correlation Matrix 
round(cor(numeric_data, use = "pairwise.complete"), 2)  
## Correlation Plot 
corrplot(cor(numeric_data, use = "pairwise.complete"), type = "lower")

#EDA
col_names <- colnames(numeric_data)

# Create a list to store density plots
density_plots <- vector("list", length = length(col_names))

# Generate density plots for each column in numeric_data
for (i in 1:length(col_names)) {
  # Create a density plot for each column
  density_plots[[i]] <- ggplot(numeric_data, aes_string(x = col_names[i])) +
    geom_density(fill = "cornflowerblue") +
    labs(title = paste("Distribution:", col_names[i]), x = "Values", y = "Density") +
    theme_minimal() +
    theme(plot.title = element_text(size = 10))  # Adjusted size from 15 to 10
}

# Combine density plots into a single view
plot_combined <- wrap_plots(density_plots, ncol = 3)
plot_combined


# Data Scaling
scaled_ds <- as.data.frame(scale(numeric_data))
head(scaled_ds)

#Principal Components Analysis (PCA)
pca_ds <- prcomp (scaled_ds)
summary(pca_ds)

pca_ds

PCA_ds <- as.data.frame(predict(pca_ds, newdata = numeric_data)[, 1:3])
summary(PCA_ds)

biplot(pca_ds, scale = 0, main = "PCA Biplot")
plot(pca_ds, type = "l", main = "Scree Plot")

#Relationship between different variables and principal components
fviz_pca_var(pca_ds,
             col.var = "contrib", 
             gradient.cols = (("Pastel2")), 
             repel = TRUE     )


#test clustering
#red: observations close tgt --> clustering 
#blue: observations far apart 
dist_euc <- get_dist(numeric_data, stand = TRUE)
fviz_dist(dist_euc)


#Elbow method
fviz_nbclust(scaled_ds , kmeans, nstart = 25, iter.max = 200, method = "wss") +
  labs(subtitle = "Elbow Method") 

#gap statistics method
fviz_nbclust(scaled_ds, FUNcluster = hcut, method = "gap_stat") + 
  labs(title = "Optimal number of clusters",
       subtitle = "Gap Statistics")

## K-Means with 3 clusters
PCA_ds_1 <- pca_ds$x[,1:3]
country <- data[c(1)]
PCA_ds_1 <- cbind(country,PCA_ds_1)
colnames(PCA_ds_1) <- c("Country","PC1","PC2","PC3")
rownames(PCA_ds_1) <- PCA_ds_1[,1]
PCA_ds_1 <- PCA_ds_1[,-1]
set.seed(631)
km <- kmeans(PCA_ds_1, PCA_ds_1[sample(1:nrow(PCA_ds_1),3),], 3)
km_cluster <- km$cluster
PCA_km <- PCA_ds_1 %>% 
  mutate(km_cluster = km_cluster)
PCA_km %>%
  group_by(km_cluster) %>%
  dplyr::summarise(n=n(), PC1=mean(PC1), PC2=mean(PC2), PC3=mean(PC3))
fviz_cluster(list(data = PCA_km, cluster = km_cluster), palette = c("red","orange","green")) + 
  ggtitle("K-Means - 3 Clusters") + 
  theme_bw()
## Cluster 1: Help Required, Cluster 2: Help Might Be Required, Cluster 3: No Help Required

PCA_km$km_cluster <- as.factor(PCA_km$km_cluster)
PCA_km$km_cluster <- revalue(PCA_km$km_cluster,
                             c("1"="Help Required", "2"="Help Might Be Required", "3"="No Help Required"))
PCA_km

## Creating World Map
world_map <- ne_countries(scale = "medium", returnclass = "sf")
PCA_km <- tibble::rownames_to_column(PCA_km, "country")
world_km <- merge(world_map, PCA_km, by.x = "admin", by.y = "country")

# Corrected plot
ggplot(data = world_km) + 
  geom_sf(aes(color = km_cluster)) +
  scale_color_manual(values = c("Help Required" = "red", 
                                "Help Might Be Required" = "orange", 
                                "No Help Required" = "green")) +
  theme_void() +
  ggtitle("K-Means Clustering: Help Required") +
  theme(plot.title = element_text(hjust = 0.5))


## Hierarchical Clustering
e_dist <- dist(PCA_ds_1, method = "euclidean")
m <- c("single", "average", "ward", "complete")
names(m) <- c("single", "average", "ward", "complete")
e_fun <- function(x) {
  agnes(e_dist, method = x)$ac
}
map_dbl(m, e_fun)
e_ward_method <- agnes(e_dist, method = "ward")
pltree(e_ward_method, cex = 0.55, hang = -1, main = "Euclidean Dendrogram using Ward's Method")

fviz_nbclust(PCA_ds_1, FUNcluster = hcut, method = "silhouette") + 
  labs(title = "Optimal number of clusters",
       subtitle = "Silhouette Analysis")
## Optimal number of clusters is 3 using silhouette statistic

fviz_nbclust(PCA_ds_1, FUNcluster = hcut, method = "wss") + 
  labs(title = "Optimal number of clusters",
       subtitle = "Sum of Square Statistic")
## Optimal number of clusters is 3 using within-clusters sum of square statistic

hc <- hclust(dist(PCA_ds_1), method = "ward.D2")
hclusters_label <- cutree(hc, k = 3)
table(hclusters_label)
data_clustered <- cbind(PCA_ds_1, hclusters_label)
head(data_clustered)
data_clustered %>%
  group_by(hclusters_label) %>%
  dplyr::summarise(n=n(), PC1=mean(PC1), PC2=mean(PC2), PC3=mean(PC3))

fviz_cluster(list(data = data_clustered, cluster = hclusters_label), palette = c("red","orange","green")) + 
  ggtitle("Hierarchical Clusters of 3")
# Cluster 1: Help Required, Cluster 2: Help Might Be Required, Cluster 3: No Help Required

data_clustered$hclusters_label <- as.factor(data_clustered$hclusters_label)
data_clustered$hclusters_label <- revalue(data_clustered$hclusters_label,
                                          c("1"="Help Required", "2"="Help Might Be Required", "3"="No Help Required"))
data_clustered

# Creating World Map
world_map <- ne_countries(scale = "medium", returnclass = "sf")
data_clustered <- tibble::rownames_to_column(data_clustered, "country")
world_hc <- merge(world_map, data_clustered, by.x="admin", by.y="country")

# Modified ggplot call
ggplot(data = world_hc) + 
  geom_sf(aes(color = hclusters_label)) +  # Use color instead of fill
  scale_color_manual(values = c("Help Required"="red", "Help Might Be Required"="orange", "No Help Required"="green")) +  # Correct function to set colors
  theme_void() +
  ggtitle("Hierarchical Clustering: Help Required") +
  theme(plot.title = element_text(hjust = 0.5))










# REGRESSION
library(tidyverse)
library(caret)
library(glmnet)
library(randomForest)
library(ranger)

# Load and inspect the dataset
df <- read.csv("California_Houses.csv")
summary(df)
head(df)

# Rename all columns to lower case for consistency
names(df) <- tolower(names(df))

# Handle missing values by imputing with the median
df[] <- lapply(df, function(x) if(is.numeric(x)) ifelse(is.na(x), median(x, na.rm = TRUE), x) else x)

# Feature scaling
numeric_features <- names(df)[sapply(df, is.numeric)]
df[numeric_features] <- scale(df[numeric_features])

# Convert categorical features to dummy variables
dummies <- dummyVars(" ~ .", data = df)
df_transformed <- predict(dummies, newdata = df)
df_transformed <- data.frame(df_transformed)

# Visualizing distributions of all numeric columns
df_long <- pivot_longer(df, cols = numeric_features)
ggplot(df_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "cornflowerblue") +
  facet_wrap(~name, scales = 'free') +
  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = NULL, y = "Frequency")

# Prepare data for regression
target <- df_transformed$median_house_value
features <- df_transformed %>% select(-median_house_value)

# Split data into training and testing sets
set.seed(42)
train_index <- createDataPartition(target, p = 0.7, list = FALSE)
x_train <- features[train_index, ]
y_train <- target[train_index]
x_test <- features[-train_index, ]
y_test <- target[-train_index]

# Train models and evaluate
models <- list(
  Linear = train(x_train, y_train, method = "lm", trControl = trainControl(method = "cv", number = 10)),
  Ridge = cv.glmnet(as.matrix(x_train), y_train, alpha = 0, lambda = 10^seq(-4, 4, length = 100)),
  Lasso = cv.glmnet(as.matrix(x_train), y_train, alpha = 1, lambda = 10^seq(-4, 4, length = 100), standardize = TRUE),
  RandomForest = train(x_train, y_train, method = "ranger", trControl = trainControl(method = "cv", number = 10), tuneLength = 5)
)

# Predict and calculate metrics
results <- list()
for (model_name in names(models)) {
  model <- models[[model_name]]
  
  if (model_name %in% c("Ridge", "Lasso")) {
    # For Ridge and Lasso, convert x_test to matrix
    predictions <- predict(model, newx = as.matrix(x_test), s = model$lambda.min)
  } else {
    # For Linear and Random Forest
    predictions <- predict(model, newdata = x_test)
  }
  
  # Print the structure of predictions
  print(paste(model_name, "predictions:"))
  print(head(predictions))
  
  # Calculate RMSE and R^2 if predictions are not NA or NULL
  if (!any(is.na(predictions)) && !is.null(predictions)) {
    rmse <- sqrt(mean((predictions - y_test)^2))
    r2 <- cor(predictions, y_test)^2
    
    results[[model_name]] <- list(RMSE = rmse, R2 = r2)
  } else {
    cat(paste("Skipping", model_name, "due to NA or NULL predictions.\n"))
  }
}

# Combine RMSE and R^2 values into a dataframe
results_df <- do.call(rbind, lapply(results, function(x) {
  data.frame(RMSE = x$RMSE, R2 = x$R2)
}))

# Add method names as a column
results_df$Method <- rownames(results_df)

# Reorder columns
results_df <- results_df[, c("Method", "RMSE", "R2")]

# Print the results dataframe
print(results_df)












# CLASSIFICATION
library(caret)
library(randomForest)
library(rpart)
library(e1071)  # For SVM
library(corrplot)
library(dplyr)
library(pROC)
library(PRROC)
library(ggplot2)
library(tictoc)
library(kernlab)

# 1. Data Loading and Exploration
df <- read.csv("/Users/ruiqichai/Desktop/SCHOOL/year 4/Machine Learning /coursework/heart.csv")
summary(df)
str(df)
any(is.na(df))

#EDA

# Define numerical and categorical columns
df_numerical <- c("Age", "RestingBP", "Cholesterol", "MaxHR", "Oldpeak")
df_categorical <- c("Sex", "ChestPainType", "FastingBS", "RestingECG", "ExerciseAngina", "ST_Slope", "HeartDisease")


df1 <- table(df$HeartDisease)
# Calculate the percentages
total <- sum(df1)
percentages <- round(df1 / total * 100, 1)  # Calculate percentage and round to 1 decimal

# Create labels with percentages for the pie chart
labels <- paste(c("No Heart Disease", "Heart Disease"), " - ", percentages, "%", sep="")

# Adjust explode to separate slices more
explode_settings <- c(0.1, 0.2)  # Increase explode for "No Heart Disease"

# Adjust starting angle
start_angle <- 135  # Adjust starting angle to help fit labels better

# Create pie chart with adjusted settings
pie(df1, labels = labels, 
    main="Percentage of Heart Diseases", col=c("skyblue", "salmon"), 
    border="black", clockwise=TRUE, init.angle=start_angle, explode=explode_settings, 
    cex.main=1.5, cex.axis=1.2, cex.lab=0.8, cex=1.2
)


numeric_data <- df[, sapply(df, is.numeric)]

correlation_matrix <- cor(numeric_data)

# Create the heatmap
corrplot(correlation_matrix, method = "color", type = "upper", 
         tl.cex = 0.7, tl.col = "black", 
         col = colorRampPalette(c("white", "blue"))(100),
         addCoef.col = "black")


# Data Scaling and Dummy Variable Creation
dummies <- dummyVars(" ~ .", data = df)
df_transformed <- predict(dummies, newdata = df)
df_transformed <- data.frame(df_transformed, HeartDisease = df$HeartDisease)

# Convert target to factor
df$HeartDisease <- factor(df$HeartDisease, levels = c(0, 1), labels = c("No", "Yes"))

set.seed(42)
index <- createDataPartition(df$HeartDisease, p = 0.7, list = TRUE)
train <- df[index$Resample1,]
test <- df[-index$Resample1,]

# Prepare training control
control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE, summaryFunction = twoClassSummary)

# Train and evaluate K-Nearest Neighbors
knn_fit <- train(HeartDisease ~ ., data = train, method = "knn", tuneLength = 5, trControl = control, metric = "Accuracy")
knn_pred <- predict(knn_fit, test)
knn_confusion <- confusionMatrix(knn_pred, test$HeartDisease)
knn_f1 <- calculateF1Score(knn_confusion)
print(paste("KNN F1 Score:", knn_f1))

# Train and evaluate Random Forest
rf_fit <- randomForest(HeartDisease ~ ., data = train, ntree = 500, mtry = 2, importance = TRUE)
rf_pred <- predict(rf_fit, test)
rf_confusion <- confusionMatrix(rf_pred, test$HeartDisease)
rf_f1 <- calculateF1Score(rf_confusion)
print(paste("Random Forest F1 Score:", rf_f1))
importance(rf_fit)
varImpPlot(rf_fit)

# Train and evaluate Support Vector Machine
svm_fit <- train(HeartDisease ~ ., data = train, method = "svmRadial", trControl = control, metric = "Accuracy", preProcess = "scale")
svm_pred <- predict(svm_fit, test)
svm_confusion <- confusionMatrix(svm_pred, test$HeartDisease)
svm_f1 <- calculateF1Score(svm_confusion)
print(paste("SVM F1 Score:", svm_f1))


# Plotting the confusion matrix
plot_confusion_matrix <- function(cm) {
  fourfoldplot(cm$table, color = c("#CC6666", "#99CC99"), conf.level = 0, main = paste("Confusion Matrix:", cm$byClass['Balanced Accuracy']))
}

plot_confusion_matrix(knn_confusion)
plot_confusion_matrix(rf_confusion)
plot_confusion_matrix(svm_confusion)

# Prepare the training control
control <- trainControl(method = "cv", number = 10, savePredictions = "final", classProbs = TRUE, summaryFunction = twoClassSummary)

# Train the models
set.seed(42)
# K-Nearest Neighbors
knn_fit <- train(HeartDisease ~ ., data = df, method = "knn", tuneLength = 5, trControl = control, metric = "ROC")
# Random Forest
rf_fit <- train(HeartDisease ~ ., data = df, method = "rf", trControl = control, metric = "ROC")
# Support Vector Machine
svm_fit <- train(HeartDisease ~ ., data = df, method = "svmRadial", trControl = control, preProcess = "scale", metric = "ROC")

# Function to plot ROC curve
plot_roc <- function(model, data) {
  # Predict using the model
  prob <- predict(model, data, type = "prob")
  roc_obj <- roc(response = data$HeartDisease, predictor = prob$Yes)
  
  # Plot
  plot(roc_obj, main = paste("ROC Curve for", deparse(substitute(model))))
  abline(a = 0, b = 1, lty = 2, col = "red")
}

# Plot ROC for each model
plot_roc(knn_fit, df)
plot_roc(rf_fit, df)
plot_roc(svm_fit, df)

# Helper function to calculate F1 Score
calculateF1Score <- function(confusionMatrix) {
  precision <- confusionMatrix$byClass['Precision']
  recall <- confusionMatrix$byClass['Recall']
  f1Score <- 2 * (precision * recall) / (precision + recall)
  return(f1Score)
}

# Initialize a dataframe to store results
model_results <- data.frame(Model = character(), F1 = numeric(), ROCAUC = numeric(), Accuracy = numeric(), TimeTaken = numeric(), stringsAsFactors = FALSE)

# Define the models and their types for prediction
models <- list("K-Nearest Neighbors" = knn_fit, 
               "Random Forest" = rf_fit, 
               "Support Vector Machine" = svm_fit)

model_types <- list("K-Nearest Neighbors" = "prob", 
                    "Random Forest" = "prob", 
                    "Support Vector Machine" = "prob")

# Train and evaluate each model
for (model_name in names(models)) {
  model <- models[[model_name]]
  pred_type <- model_types[[model_name]]
  
  start_time <- Sys.time()
  
  # Check if the model is a random forest
  if (model_name == "Random Forest") {
    predictions <- predict(model, test, type = "response")
    prob_predictions <- predict(model, test, type = "prob")[,2]
  } else {
    predictions <- predict(model, test, type = "raw")
    prob_predictions <- predict(model, test, type = pred_type)[,2]
  }
  
  end_time <- Sys.time()
  time_taken <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  confusion_mat <- confusionMatrix(as.factor(predictions), test$HeartDisease)
  f1_score <- calculateF1Score(confusion_mat)
  accuracy <- confusion_mat$overall['Accuracy']
  roc_auc <- roc(test$HeartDisease, prob_predictions)$auc
  
  model_results <- rbind(model_results, data.frame(Model = model_name, F1 = f1_score, ROCAUC = roc_auc, Accuracy = accuracy, TimeTaken = time_taken))
}

# Print the consolidated model performance
print(model_results)
