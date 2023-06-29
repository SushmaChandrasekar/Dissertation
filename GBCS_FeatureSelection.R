###--------------German Breast Cancer Study------------------###

# Load the required packages
library(randomForest)
library(ggplot2)

# Load the dataset
load("D:/SUSHMA DISSERTATION/GBCS.rdata")
ls()
gbcs_data = na.omit(gbcs)

# Specify the transformations for each feature
transformations <- list(
  age = I((gbcs_data$age/100)^-1) + I((gbcs_data$age/100)^-1 * log((gbcs_data$age/100))),
  size = gbcs_data$size,
  nodes = I((gbcs_data$nodes/10)^0.5),
  prog_recp = I(((gbcs_data$prog_recp+1)/100)^1),
  estrg_recp = gbcs_data$estrg_recp
)

Y = as.factor(gbcs_data$censrec)
# Create a new dataset with transformed features
transformed_data <- data.frame(transformations)

# Train the Random Forest model
rf_model <- randomForest(transformed_data, Y, ntree = 500)

# Extract feature importance
importance <- importance(rf_model)

# Set a threshold for feature selection (e.g., select variables with importance > 0)
selected_features <- rownames(importance)[importance > 0]

print(selected_features)

# Generate variable importance plot
varImpPlot(rf_model, main = "Variable Importance Plot")



##############################################################################
###-----------Feature Selection using Lasso, Ridge and Elastic Net---------###
##############################################################################

# Load the required packages
library(glmnet)

# Load the dataset
load("D:/SUSHMA DISSERTATION/GBCS.rdata")
ls()
gbcs_data <- na.omit(gbcs)

# Specify the transformations for each feature
transformations <- list(
  age = I((gbcs_data$age/100)^-1) + I((gbcs_data$age/100)^-1 * log((gbcs_data$age/100))),
  size = gbcs_data$size,
  nodes = I((gbcs_data$nodes/10)^0.5),
  prog_recp = I(((gbcs_data$prog_recp+1)/100)^1),
  estrg_recp = gbcs_data$estrg_recp,
  censrec = as.factor(gbcs_data$censrec)
)

selected_data <- data.frame(transformations)

# Split the data into training and testing sets
set.seed(123)  # For reproducibility
train_indices <- sample(1:nrow(selected_data), nrow(selected_data) * 0.8)  # 80% for training
train_data <- selected_data[train_indices, ]
test_data <- selected_data[-train_indices, ]

# Fit the Lasso regression model
lasso_lambda <- cv.glmnet(as.matrix(train_data[, -6]), train_data$censrec, alpha = 1, family = "binomial")

# Get the optimal lambda value for Lasso
opt_lambda_lasso <- lasso_lambda$lambda.min

# Fit the final Lasso model with the optimal lambda
final_model_lasso <- glmnet(as.matrix(train_data[, -6]), train_data$censrec, alpha = 1, family = "binomial", lambda = opt_lambda_lasso)

# Fit the Ridge regression model
ridge_lambda <- cv.glmnet(as.matrix(train_data[, -6]), train_data$censrec, alpha = 0, family = "binomial")

# Get the optimal lambda value for Ridge
opt_lambda_ridge <- ridge_lambda$lambda.min

# Fit the final Ridge model with the optimal lambda
final_model_ridge <- glmnet(as.matrix(train_data[, -6]), train_data$censrec, alpha = 0, family = "binomial", lambda = opt_lambda_ridge)

# Fit the Elastic Net regression model
enet_lambda <- cv.glmnet(as.matrix(train_data[, -6]), train_data$censrec, alpha = 0.5, family = "binomial")

# Get the optimal lambda value for Elastic Net
opt_lambda_enet <- enet_lambda$lambda.min

# Fit the final Elastic Net model with the optimal lambda
final_model_enet <- glmnet(as.matrix(train_data[, -6]), train_data$censrec, alpha = 0.5, family = "binomial", lambda = opt_lambda_enet)

# Predict on the test data for Lasso
predictions_lasso <- predict(final_model_lasso, newx = as.matrix(test_data[, -6]), type = "class")

# Calculate the accuracy for Lasso
accuracy_lasso <- sum(predictions_lasso == test_data$censrec) / length(predictions_lasso)
cat("Accuracy (Lasso):", accuracy_lasso, "\n")

# Predict on the test data for Ridge
predictions_ridge <- predict(final_model_ridge, newx = as.matrix(test_data[, -6]), type = "class")

# Calculate the accuracy for Ridge
accuracy_ridge <- sum(predictions_ridge == test_data$censrec) / length(predictions_ridge)
cat("Accuracy (Ridge):", accuracy_ridge, "\n")

# Predict on the test data for Elastic Net
predictions_enet <- predict(final_model_enet, newx = as.matrix(test_data[, -6]), type = "class")

# Calculate the accuracy for Elastic Net
accuracy_enet <- sum(predictions_enet == test_data$censrec) / length(predictions_enet)
cat("Accuracy (Elastic Net):", accuracy_enet, "\n")

# Print the coefficients for Lasso
coef(final_model_lasso)

# Print the coefficients for Ridge
coef(final_model_ridge)

# Print the coefficients for Elastic Net
coef(final_model_enet)

                                                                       