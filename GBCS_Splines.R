
###--------------German Breast Cancer Study------------------###


################################################################
###--------------Optimal number of knots for AGE--------------##
################################################################

# Load necessary libraries
library(mgcv)
library(pROC) 

load("D:/SUSHMA DISSERTATION/GBCS.rdata")
gbcs_data = na.omit(gbcs)

# Consider a range of possible knots
knots_range <- c(3, 5, 7, 10, 15, 20)

# Transform categorical response variable into a factor
gbcs_data$censrec <- as.factor(gbcs_data$censrec)

# Create a function to calculate AUC for a given number of knots
calc_auc <- function(knots) {
  spline_model <- gam(as.formula(paste("censrec ~ s(age, k=", knots, ")", sep="")), family=binomial(), data=gbcs_data)
  preds <- predict(spline_model, newdata=gbcs_data, type="response")
  auc(roc(gbcs_data$censrec, preds))
}

# Apply the function to each number of knots
cv_errors <- sapply(knots_range, calc_auc)

# Print the AUC for each number of knots
print(cv_errors)

# Determine the number of knots with the highest AUC
optimal_knots <- knots_range[which.max(cv_errors)]
print(paste("Optimal number of knots: ", optimal_knots))

################################################################
###--------------Penalized Spline Model for AGE---------------##
################################################################

# Fit penalized spline model
spline_model <- gam(censrec ~ s(age, k=15), family=binomial, data=gbcs_data)

# Inspect the model
print(summary(spline_model))

# Plot spline with confidence intervals
plot(spline_model, shade=TRUE)

# Predict with confidence intervals
preds <- predict(spline_model, newdata=gbcs_data, type="link", se.fit=TRUE)
lower_bound <- preds$fit - 1.96 * preds$se.fit
upper_bound <- preds$fit + 1.96 * preds$se.fit



################################################################
###--------------Optimal number of knots for SIZE-------------##
################################################################

load("D:/SUSHMA DISSERTATION/GBCS.rdata")
gbcs_data = na.omit(gbcs)

# Consider a range of possible knots
knots_range <- c(20, 30, 35, 38, 40)

# Transform categorical response variable into a factor
gbcs_data$censrec <- as.factor(gbcs_data$censrec)

# Create a function to calculate AUC for a given number of knots
calc_auc <- function(knots) {
  spline_model <- gam(as.formula(paste("censrec ~ s(size, k=", knots, ")", sep="")), family=binomial(), data=gbcs_data)
  preds <- predict(spline_model, newdata=gbcs_data, type="response")
  auc(roc(gbcs_data$censrec, preds))
}

# Apply the function to each number of knots
cv_errors <- sapply(knots_range, calc_auc)

# Print the AUC for each number of knots
print(cv_errors)

# Determine the number of knots with the highest AUC
optimal_knots <- knots_range[which.max(cv_errors)]
print(paste("Optimal number of knots: ", optimal_knots))

################################################################
###--------------Penalized Spline Model for SIZE--------------##
################################################################

# Fit penalized spline model
spline_model <- gam(censrec ~ s(size, k=30), family=binomial, data=gbcs_data)

# Inspect the model
print(summary(spline_model))

# Plot spline with confidence intervals
plot(spline_model, shade=TRUE)

# Predict with confidence intervals
preds <- predict(spline_model, newdata=gbcs_data, type="link", se.fit=TRUE)
lower_bound <- preds$fit - 1.96 * preds$se.fit
upper_bound <- preds$fit + 1.96 * preds$se.fit

################################################################
###------------Optimal number of knots for NODES--------------##
################################################################

load("D:/SUSHMA DISSERTATION/GBCS.rdata")
gbcs_data <- na.omit(gbcs)

# Consider a range of possible knots
knots_range <- c(5, 10, 15, 20)

# Transform categorical response variable into a factor
gbcs_data$censrec <- as.factor(gbcs_data$censrec)

# Create a function to calculate AUC for a given number of knots
calc_auc <- function(knots) {
  spline_model <- gam(as.formula(paste("censrec ~ s(nodes, k=", knots, ")", sep="")), family=binomial(), data=gbcs_data)
  preds <- predict(spline_model, newdata=gbcs_data, type="response")
  auc(roc(gbcs_data$censrec, preds))
}

# Apply the function to each number of knots
cv_errors <- sapply(knots_range, calc_auc)

# Print the AUC for each number of knots
print(cv_errors)

# Determine the number of knots with the highest AUC
optimal_knots <- knots_range[which.max(cv_errors)]
print(paste("Optimal number of knots: ", optimal_knots))


################################################################
###------------Penalized Spline Model for NODES---------------##
################################################################

# Fit penalized spline model
spline_model <- gam(censrec ~ s(nodes, k=15), family=binomial, data=gbcs_data)

# Inspect the model
print(summary(spline_model))

# Plot spline with confidence intervals
plot(spline_model, shade=TRUE)

# Predict with confidence intervals
preds <- predict(spline_model, newdata=gbcs_data, type="link", se.fit=TRUE)
lower_bound <- preds$fit - 1.96 * preds$se.fit
upper_bound <- preds$fit + 1.96 * preds$se.fit

################################################################
###-----------Optimal number of knots for PROG_RECP-----------##
################################################################

load("D:/SUSHMA DISSERTATION/GBCS.rdata")
gbcs_data <- na.omit(gbcs)

# Consider a range of possible knots
knots_range <- c(5, 10, 15, 20, 25, 30, 35)

# Transform categorical response variable into a factor
gbcs_data$censrec <- as.factor(gbcs_data$censrec)

# Create a function to calculate AUC for a given number of knots
calc_auc <- function(knots) {
  spline_model <- gam(as.formula(paste("censrec ~ s(prog_recp, k=", knots, ")", sep="")), family=binomial(), data=gbcs_data)
  preds <- predict(spline_model, newdata=gbcs_data, type="response")
  auc(roc(gbcs_data$censrec, preds))
}

# Apply the function to each number of knots
cv_errors <- sapply(knots_range, calc_auc)

# Print the AUC for each number of knots
print(cv_errors)

# Determine the number of knots with the highest AUC
optimal_knots <- knots_range[which.max(cv_errors)]
print(paste("Optimal number of knots: ", optimal_knots))

################################################################
###-----------Penalized Spline Model for PROG_RECP------------##
################################################################


# Fit penalized spline model
spline_model <- gam(censrec ~ s(prog_recp, k=25), family=binomial, data=gbcs_data)

# Inspect the model
print(summary(spline_model))

# Plot spline with confidence intervals
plot(spline_model, shade=TRUE)

# Predict with confidence intervals
preds <- predict(spline_model, newdata=gbcs_data, type="link", se.fit=TRUE)
lower_bound <- preds$fit - 1.96 * preds$se.fit
upper_bound <- preds$fit + 1.96 * preds$se.fit


################################################################
###-----------Optimal number of knots for ESTRG_RECP----------##
################################################################

load("D:/SUSHMA DISSERTATION/GBCS.rdata")
gbcs_data <- na.omit(gbcs)

# Consider a range of possible knots
knots_range <- c(5, 10, 15, 20, 25, 30, 33, 35)

# Transform categorical response variable into a factor
gbcs_data$censrec <- as.factor(gbcs_data$censrec)

# Create a function to calculate AUC for a given number of knots
calc_auc <- function(knots) {
  spline_model <- gam(as.formula(paste("censrec ~ s(estrg_recp, k=", knots, ")", sep="")), family=binomial(), data=gbcs_data)
  preds <- predict(spline_model, newdata=gbcs_data, type="response")
  auc(roc(gbcs_data$censrec, preds))
}

# Apply the function to each number of knots
cv_errors <- sapply(knots_range, calc_auc)

# Print the AUC for each number of knots
print(cv_errors)

# Determine the number of knots with the highest AUC
optimal_knots <- knots_range[which.max(cv_errors)]
print(paste("Optimal number of knots: ", optimal_knots))


################################################################
###-----------Penalized Spline Model for ESTRG_RECP-----------##
################################################################


# Fit penalized spline model
spline_model <- gam(censrec ~ s(estrg_recp, k=20), family=binomial, data=gbcs_data)

# Inspect the model
print(summary(spline_model))

# Plot spline with confidence intervals
plot(spline_model, shade=TRUE)

# Predict with confidence intervals
preds <- predict(spline_model, newdata=gbcs_data, type="link", se.fit=TRUE)
lower_bound <- preds$fit - 1.96 * preds$se.fit
upper_bound <- preds$fit + 1.96 * preds$se.fit

################################################################
###----------------Feature Selection - RFE--------------------##
################################################################

# Install 'caret' package if not already installed
# install.packages("caret")

# Load required libraries
library(caret)
library(mgcv)

# Set the seed for reproducibility
set.seed(123)

# Create dummy variables for "hormone" and "grade"
hormone_dummies <- model.matrix(~ hormone - 1, data = gbcs_data)
grade_dummies <- model.matrix(~ grade - 1, data = gbcs_data)

# Create a data matrix with the predictor variables
# Combine the predictor variables and their dummy variables
x <- cbind(gbcs_data[, c("age", "size", "nodes", "prog_recp", "estrg_recp")], hormone_dummies, grade_dummies)


# Define the formula for the model
formula <- "censrec ~ s(age) + s(size) + s(nodes) + s(prog_recp) + s(estrg_recp) + hormone + grade"

# Create the control function for RFE
ctrl <- rfeControl(functions = rfFuncs, method = "cv", number = 5)

# Perform RFE
rfe_results <- rfe(x = x, y = gbcs_data$censrec, sizes = c(1, 2, 3, 4, 5), rfeControl = ctrl, 
                   method = "gam", tuneGrid = expand.grid(k = 1:5))

# Print the selected features
print(rfe_results$optVariables)





