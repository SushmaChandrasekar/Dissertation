# Load the required packages
library(randomForest)
library(caret)
library(pROC)

# Load the dataset
load("D:/SUSHMA DISSERTATION/GBCS.rdata")
ls()
gbcs_data <- na.omit(gbcs)

# Create a new dataset without transformations
data <- data.frame(
  age = gbcs_data$age,
  size = gbcs_data$size,
  nodes = gbcs_data$nodes,
  prog_recp = gbcs_data$prog_recp,
  estrg_recp = gbcs_data$estrg_recp,
  hormone = as.factor(gbcs_data$hormone),
  grade = as.factor(gbcs_data$grade),
  censrec = as.factor(gbcs_data$censrec)
)

# Set the seed for reproducibility
set.seed(6051)

# Split the data into training and testing sets
train_indices <- createDataPartition(data$censrec, p = 0.8, list = FALSE)
train_data <- data[train_indices, ]
test_data <- data[-train_indices, ]

# Specify the response variable
Y_train <- train_data$censrec
Y_test <- test_data$censrec

# Set up the cross-validation control
control <- trainControl(method = "cv", number = 10)

# Train the Random Forest model using cross-validation
rf_model <- train(x = train_data[, -8], y = Y_train, method = "rf", trControl = control, ntree = 500)

# Perform predictions on the test data
test_predictions <- predict(rf_model, newdata = test_data[, -8], type = "prob")

# Perform predictions on the test data
test_predictions_class <- predict(rf_model, newdata = test_data[, -8], type = "raw")

# Calculate the AUC on the test data
test_auc <- roc(as.vector(Y_test), as.vector(test_predictions[, 2]))$auc

# Print the results
cat("Test AUC:", test_auc, "\n")

# Plot the ROC curve
roc_obj <- roc(as.vector(Y_test), as.vector(test_predictions[, 2]))
plot(roc_obj, main = "Receiver Operating Characteristic (ROC) Curve", print.auc = TRUE)


# Calculate other evaluation metrics
confusion_matrix <- confusionMatrix(test_predictions_class, Y_test, positive = "1")
recall <- confusion_matrix$byClass["Recall"]
precision <- confusion_matrix$byClass["Precision"]
f1_score <- confusion_matrix$byClass["F1"]

# Print the results
cat("Recall:", recall, "\n")
cat("Precision:", precision, "\n")
cat("F1 Score:", f1_score, "\n")

