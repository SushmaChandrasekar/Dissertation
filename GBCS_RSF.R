# Load necessary libraries
library(survival)
library(randomForestSRC)
library(pec)
library(ggplot2)
library(survminer)

##############################################################################
#---------------------------Random Survival Forest---------------------------#
##############################################################################
library(survival)
# Load the dataset
load("D:/SUSHMA DISSERTATION/GBCS.rdata")
ls()
df <- na.omit(gbcs)
# Remove the 'id' column
df$id <- NULL

# Convert the categorical variables to factors
df$hormone <- as.factor(df$hormone)
df$grade <- as.factor(df$grade)

# Split the data into training and test set
set.seed(6500) # Setting seed for reproducibility
train_index <- sample(1:nrow(df), 0.7*nrow(df)) # 70% of data goes to the training set

train_df <- df[train_index, ]
test_df <- df[-train_index, ]

# Fit a Random Survival Forest model on training data
rsf_model <- rfsrc(Surv(rectime, censrec) ~ ., data = train_df, nsplit = 3, ntree = 1000, time.interest = c(100, 500, 1000, 1500, 2000), forest = TRUE)

# Backward variable selection in the Cox regression model
fitform <- Surv(rectime,censrec) ~ age + size + nodes + grade + hormone + 
  prog_recp + estrg_recp 
cox_model <- selectCox(fitform, data = train_df, rule = "aic")

# Prediction and Error calculation on the test set
rsf_pred <- pec(rsf_model, data = test_df, formula = Surv(rectime, censrec) ~ 1)
# For the Cox model
cox_pred <- pec(cox_model, data = test_df, formula = Surv(rectime, censrec) ~ 1)

fitpec <- pec(list("selectcox" = cox_model,
                   "rsf" = rsf_model),
              data = train_df,
              formula = Surv(rectime, censrec) ~ 1,
              splitMethod = "BootCvErr",
              B = 1000,
              M = 350,
              keep.index = TRUE,
              keep.matrix = TRUE)
plot(fitpec)

## Variable Importance Plot

oo <- subsample(rsf_model, verbose = FALSE)

vimpCI <- extract.subsample(oo)$var.jk.sel.Z

# Confidence Intervals for VIMP
plot.subsample(oo)

## Brier and CRPS score

# Calculate Brier scores for Kaplan-Meier and RSF censoring distribution estimators
bs.km <- get.brier.survival(rsf_model, cens.mode = "km")$brier.score
bs.rsf <- get.brier.survival(rsf_model, cens.mode = "rfsrc")$brier.score


## plot the brier score
plot(bs.km, type = "s", col = 2)
lines(bs.rsf, type ="s", col = 4)
legend("bottomright", legend = c("cens.model = km", "cens.model = rfsrc"), fill = c(2,4))

# The helper function `trapz` for trapezoidal integration
trapz <- randomForestSRC:::trapz
time <- rsf_model$time.interest

# Compute the CRPS for every time point
crps.km <- sapply(1:length(time), function(j) {
  trapz(time[1:j], bs.km[1:j, 2] / diff(range(time[1:j])))
})
crps.rsf <- sapply(1:length(time), function(j) {
  trapz(time[1:j], bs.rsf[1:j, 2] / diff(range(time[1:j])))
})

# Plot CRPS as function of time
plot(time, crps.km, ylab = "CRPS", type = "s", col = 2)
lines(time, crps.rsf, type ="s", col = 4)
legend("bottomright", legend=c("cens.model = km", "cens.model = rfsrc"), fill=c(2,4))
