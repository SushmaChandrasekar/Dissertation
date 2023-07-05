# Load the required packages
library(glmnet)
library(caret)
library(pROC)

# Load the dataset
load("D:/SUSHMA DISSERTATION/GBCS.rdata")
ls()
gbcs_data <- na.omit(gbcs)

# Specify the transformations for each feature
transformations <- list(
  age1 = I((gbcs_data$age/100)^-1),
  age2 = I((gbcs_data$age/100)^-1 * log((gbcs_data$age/100))),
  size = gbcs_data$size,
  nodes = I((gbcs_data$nodes/10)^0.5),
  prog_recp = I(((gbcs_data$prog_recp+1)/100)^1),
  estrg_recp = gbcs_data$estrg_recp,
  censrec = as.factor(gbcs_data$censrec)
)

selected_data <- data.frame(transformations)

# Split the data into training and testing sets
set.seed(6500)  # For reproducibility
train_indices <- sample(1:nrow(selected_data), nrow(selected_data) * 0.8)  # 80% for training
train_data <- selected_data[train_indices, ]
test_data <- selected_data[-train_indices, ]

# Standardize the predictor variables
train_data_scaled <- scale(train_data[, -7])  # Exclude the response variable
test_data_scaled <- scale(test_data[, -7])  # Exclude the response variable

# Convert the scaled data back to a data frame
train_data_scaled <- as.data.frame(train_data_scaled)
test_data_scaled <- as.data.frame(test_data_scaled)

# Add the standardized response variable back to the data frames
train_data_scaled$censrec <- train_data$censrec
test_data_scaled$censrec <- test_data$censrec
##############################################################################
# Fit the Lasso regression model with standardized data
##############################################################################
lasso_lambda <- cv.glmnet(as.matrix(train_data_scaled[, -7]), train_data_scaled$censrec, alpha = 1, family = "binomial")
opt_lambda_lasso <- lasso_lambda$lambda.min
final_model_lasso <- glmnet(as.matrix(train_data_scaled[, -7]), train_data_scaled$censrec, alpha = 1, family = "binomial", lambda = opt_lambda_lasso)

# Predict on the standardized test data for Lasso
predictions_lasso <- predict(final_model_lasso, newx = as.matrix(test_data_scaled[, -7]), type = "class")
accuracy_lasso <- sum(predictions_lasso == test_data_scaled$censrec) / length(predictions_lasso)
cat("Accuracy (Lasso, standardized):", accuracy_lasso, "\n")
# Print the standardized coefficients for Lasso
coef(final_model_lasso)

# Convert predicted values to factor with same levels as actual values
predictions_lasso <- factor(predictions_lasso, levels = levels(test_data_scaled$censrec))

# Calculate the confusion matrix for Lasso
confusion_matrix_lasso <- confusionMatrix(predictions_lasso, test_data_scaled$censrec, positive = "1")
print(confusion_matrix_lasso)

# Calculate other accuracy metrics for Lasso
recall_lasso <- confusion_matrix_lasso$byClass["Recall"]
precision_lasso <- confusion_matrix_lasso$byClass["Precision"]
f1_score_lasso <- confusion_matrix_lasso$byClass["F1"]

# Convert factor predictions to numeric probabilities
predictions_lasso_numeric <- as.numeric(predictions_lasso) - 1
# Calculate the AUC for Lasso
auc_lasso <- roc(test_data_scaled$censrec, predictions_lasso_numeric)$auc

cat("Recall (Lasso):", recall_lasso, "\n")
cat("Precision (Lasso):", precision_lasso, "\n")
cat("F1 Score (Lasso):", f1_score_lasso, "\n")
cat("AUC (Lasso):", auc_lasso, "\n")
##############################################################################
# Fit the Ridge regression model with standardized data
##############################################################################
ridge_lambda <- cv.glmnet(as.matrix(train_data_scaled[, -7]), train_data_scaled$censrec, alpha = 0, family = "binomial")
opt_lambda_ridge <- ridge_lambda$lambda.min
final_model_ridge <- glmnet(as.matrix(train_data_scaled[, -7]), train_data_scaled$censrec, alpha = 0, family = "binomial", lambda = opt_lambda_ridge)

# Predict on the standardized test data for Ridge
predictions_ridge <- predict(final_model_ridge, newx = as.matrix(test_data_scaled[, -7]), type = "class")
accuracy_ridge <- sum(predictions_ridge == test_data_scaled$censrec) / length(predictions_ridge)
cat("Accuracy (Ridge, standardized):", accuracy_ridge, "\n")
# Print the standardized coefficients for Ridge
coef(final_model_ridge)

# Convert predicted values to factor with same levels as actual values
predictions_ridge <- factor(predictions_ridge, levels = levels(test_data_scaled$censrec))
confusion_matrix_ridge <- confusionMatrix(predictions_ridge, test_data_scaled$censrec, positive = "1")
print(confusion_matrix_ridge)

# Calculate other accuracy metrics for Ridge
recall_ridge <- confusion_matrix_ridge$byClass["Recall"]
precision_ridge <- confusion_matrix_ridge$byClass["Precision"]
f1_score_ridge <- confusion_matrix_ridge$byClass["F1"]
# Convert factor predictions to numeric probabilities
predictions_ridge_numeric <- as.numeric(predictions_ridge) - 1
# Calculate the AUC for Lasso
auc_ridge <- roc(test_data_scaled$censrec, predictions_ridge_numeric)$auc
cat("Recall (Ridge):", recall_ridge, "\n")
cat("Precision (Ridge):", precision_ridge, "\n")
cat("F1 Score (Ridge):", f1_score_ridge, "\n")
cat("AUC (Ridge):", auc_ridge, "\n")

##############################################################################
# Fit the Elastic Net regression model with standardized data
##############################################################################
enet_lambda <- cv.glmnet(as.matrix(train_data_scaled[, -7]), train_data_scaled$censrec, alpha = 0.5, family = "binomial")
opt_lambda_enet <- enet_lambda$lambda.min
final_model_enet <- glmnet(as.matrix(train_data_scaled[, -7]), train_data_scaled$censrec, alpha = 0.5, family = "binomial", lambda = opt_lambda_enet)

# Predict on the standardized test data for Elastic Net
predictions_enet <- predict(final_model_enet, newx = as.matrix(test_data_scaled[, -7]), type = "class")
accuracy_enet <- sum(predictions_enet == test_data_scaled$censrec) / length(predictions_enet)
cat("Accuracy (Elastic Net, standardized):", accuracy_enet, "\n")
# Print the standardized coefficients for Elastic Net
coef(final_model_enet)

# Convert predicted values to factor with same levels as actual values
predictions_enet <- factor(predictions_enet, levels = levels(test_data_scaled$censrec))
confusion_matrix_enet <- confusionMatrix(predictions_enet, test_data_scaled$censrec, positive = "1")
print(confusion_matrix_enet)

# Calculate other accuracy metrics for Elastic Net
recall_enet <- confusion_matrix_enet$byClass["Recall"]
precision_enet <- confusion_matrix_enet$byClass["Precision"]
f1_score_enet <- confusion_matrix_enet$byClass["F1"]
# Convert factor predictions to numeric probabilities
predictions_enet_numeric <- as.numeric(predictions_enet) - 1
# Calculate the AUC for Lasso
auc_enet <- roc(test_data_scaled$censrec, predictions_enet_numeric)$auc
cat("Recall (Elastic Net):", recall_enet, "\n")
cat("Precision (Elastic Net):", precision_enet, "\n")
cat("F1 Score (Elastic Net):", f1_score_enet, "\n")
cat("AUC (Elastic Net):", auc_enet, "\n")

