# Load necessary libraries
library(survival)
library(randomForestSRC)
library(pec)
library(ggplot2)
library(survminer)

##############################################################################
#----------------Random Survival Forest and Cox Model------------------------#
##############################################################################

# Load the dataset
load("D:/SUSHMA DISSERTATION/GBCS.rdata")
ls()
df <- na.omit(gbcs)

# Convert the categorical variables to factors
df$hormone <- as.factor(df$hormone)
df$grade <- as.factor(df$grade)

# Create a survival object
SurvObj <- Surv(df$rectime, df$censrec)

# Fit a Cox proportional hazards model
cox_model <- coxph(SurvObj ~ age + nodes + size + prog_recp + estrg_recp + hormone + grade, data = df, x = TRUE)


# Fit a Random Survival Forest model
rsf_model <- rfsrc(Surv(rectime, censrec) ~ ., data = df, nsplit = 3, ntree = 100, time.interest = c(100, 500, 1000, 1500, 2000))


# prediction error curve function from pec library
# For the Cox model
cox_pred <- pec(cox_model, data = df, formula = Surv(rectime, censrec) ~ 1)

# For the Random Survival Forest model
rsf_pred <- pec(rsf_model, data = df, formula = Surv(rectime, censrec) ~ 1)

cox_errors <- cox_pred$AppErr$coxph
rsf_errors <- rsf_pred$AppErr$rfsrc
timepoints <- cox_pred$time

error_df <- data.frame(
  Time = rep(timepoints, 2),
  Error = c(cox_errors, rsf_errors),
  Model = rep(c("Cox Model", "Random Survival Forest"), each=length(timepoints))
)

ggplot(error_df, aes(x=Time, y=Error, colour=Model)) +
  geom_line() +
  xlab("Time, Days") +
  ylab("Prediction Error") +
  scale_color_manual("", breaks = c("Cox Model", "Random Survival Forest"), values = c("red", "blue")) +
  theme_minimal()


##############################################################################
#-------------------Log-rank test for  grade variable------------------------#
##############################################################################

# Create a survival object
SurvObj <- Surv(df$rectime, df$censrec)

# Perform a log-rank test to compare survival distributions between grades
log_rank_test <- survdiff(SurvObj ~ df$grade)

# Print log-rank test statistics
print(log_rank_test)
?rfsrc

##############################################################################
#----------------Kaplan-Meier survival curve for each grade------------------#
##############################################################################


# Create a survival object
SurvObj <- Surv(df$rectime, df$censrec)

# Fit a Kaplan-Meier survival curve for each grade
km_fit <- survfit(SurvObj ~ grade, data = df)

# Plot the survival curves
ggsurvplot(
  km_fit,
  data = df,
  risk.table = TRUE, # Add risk table
  pval = TRUE, # Add p-value
  conf.int = TRUE, # Add confidence intervals
  legend.title = "Grade",
  xlab = "Time, Days",
  ylab = "Survival Probability",
  title = "Kaplan-Meier Survival Curves by Grade",
  palette = c("blue", "red", "green") 
)

summary(km_fit)

