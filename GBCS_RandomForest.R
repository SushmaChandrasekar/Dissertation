##############################################################################
#-------------------------Random Forest model -------------------------------#
##############################################################################

# Load the required packages
library(randomForest)
library(caret)
library(pROC)
library(PRROC)

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
test_predictions_rf <- predict(rf_model, newdata = test_data[, -8], type = "prob")

# Perform predictions on the test data
test_predictions_class <- predict(rf_model, newdata = test_data[, -8], type = "raw")

# Calculate the AUC with CI on the test data
roc_obj <- roc(as.vector(Y_test), as.vector(test_predictions_rf[, 2]))
auc_with_ci <- ci.auc(roc_obj, conf.level = 0.95)

# Print the AUC with CI
cat("Test AUC:", round(roc_obj$auc, 3), "\n")
cat("AUC CI:", round(auc_with_ci[1], 3), "-", round(auc_with_ci[3], 3), "\n")

# Plot the ROC curve
roc_obj <- roc(as.vector(Y_test), as.vector(test_predictions_rf[, 2]))
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

# Extract variable importance from the model
var_imp <- varImp(rf_model, scale = FALSE)

# Print the variable importance
print(var_imp)

# Plot the variable importance
plot(var_imp)


##############################################################################
#----------------Logistic Regression with Elastic Net penalty----------------#
##############################################################################



# Load the required package
library(glmnet)

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

# Convert the response variable to numeric
Y_train <- as.numeric(train_data$censrec) - 1
Y_test <- as.numeric(test_data$censrec) - 1

# Convert categorical variables into dummy variables (binary 0 and 1)
# "-1" is to exclude the intercept term
X_train <- model.matrix(~.-1, data = train_data[,-8])
X_test <- model.matrix(~.-1, data = test_data[,-8])

# Train the logistic regression model with elastic net penalty
logistic_model <- cv.glmnet(X_train, Y_train, family = "binomial", alpha = 0.5, type.measure = "auc", nfolds = 10, standardize = TRUE)

# Perform predictions on the test data
test_predictions <- predict(logistic_model, newx = X_test, s = logistic_model$lambda.min, type = "response")


# Calculate the AUC with CI on the test data
roc_obj <- roc(as.vector(Y_test), as.vector(test_predictions))
auc_with_ci <- ci.auc(roc_obj, conf.level = 0.95)

# Print the AUC with CI
cat("Test AUC:", round(roc_obj$auc, 3), "\n")
cat("AUC CI:", round(auc_with_ci[1], 3), "-", round(auc_with_ci[3], 3), "\n")

# Plot the ROC curve with AUC and CI

plot(roc_obj, main = "Receiver Operating Characteristic (ROC) Curve", print.auc = TRUE)

# Plot AUC for each lambda value
plot(logistic_model$lambda, logistic_model$cvm, type = "p", log = "x",
     xlab = "Log(Lambda)", ylab = "Cross-Validated AUC",
     main = "Cross-Validated AUC for Different Lambda Values")


# Create a data frame with lambda and AUC values
df <- data.frame(lambda = logistic_model$lambda, AUC = logistic_model$cvm)

# Find the row with the maximum AUC
max_row <- which.max(df$AUC)

# Find the maximum AUC
max_AUC <- df$AUC[max_row]

# Create the base plot
plot(df$lambda, df$AUC, type = "p", log = "x", xlab = "Log(Lambda)", 
     ylab = "Cross-Validated AUC", main = "Cross-Validated AUC for Different Lambda Values",
     col = ifelse(1:nrow(df) == max_row, "red", "black"), # Change the color of the maximum AUC point to red
     pch = ifelse(1:nrow(df) == max_row, 19, 1), # Change the shape of the maximum AUC point to a solid circle
     cex = ifelse(1:nrow(df) == max_row, 2, 1)) # Change the size of the maximum AUC point to be larger

# Add a text to display the maximum AUC
text(df$lambda[max_row], df$AUC[max_row]- 0.005, labels = paste("Max AUC =", round(max_AUC, 3)), pos = 4, cex = 0.8)

# Add a legend
legend("bottomright", legend = c("Maximum AUC", "Other values"), pch = c(19, 1), col = c("red", "black"))

##############################################################################
#--------------------------Bland Altman Analysis-----------------------------#
##############################################################################



library(blandr)
library(ggplot2)

# Create a data frame with the required columns
table_data <- data.frame(
  SNo = 1:length(test_predictions_rf[, 2]),
  test_pred_rf = test_predictions_rf[, 2],
  test_pred_en = round(test_predictions[, 1], 3),
  Mean = round(rowMeans(cbind(test_predictions_rf[, 2], test_predictions[, 1])), 3),
  Difference = round(abs(test_predictions_rf[, 2] - test_predictions[, 1]),3)
)

# Print the table
print(table_data)


blandr.draw( test_predictions_rf[, 2] , test_predictions[, 1] )
blandr.statistics( test_predictions_rf[, 2] , test_predictions[, 1] )


differences = round(abs(test_predictions_rf[, 2] - test_predictions[, 1]),3)

# Calculate mean difference
mean_difference <- mean(differences)

# Calculate standard deviation of differences
std_deviation <- sd(differences)

# Calculate upper and lower limits of agreement (LOA)
upper_loa <- mean_difference + (2 * std_deviation)
lower_loa <- mean_difference - (2 * std_deviation)

# Calculate range
range_2s <- upper_loa - lower_loa

# Count data points within ± 2s
within_2s_count <- sum(differences >= lower_loa & differences <= upper_loa)

# Calculate percentage within ± 2s
percentage_within_2s <- (within_2s_count / length(differences)) * 100

# Print the results
cat("Mean Difference:", mean_difference, "\n")
cat("Standard Deviation:", std_deviation, "\n")
cat("Upper LOA:", upper_loa, "\n")
cat("Lower LOA:", lower_loa, "\n")
cat("Range within ± 2s:", range_2s, "\n")
cat("Number of Data Points within ± 2s:", within_2s_count, "\n")
cat("Percentage within ± 2s:", percentage_within_2s, "\n")
