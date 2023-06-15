
###--------------German Breast Cancer Study------------------###


# Load the required packages
library(mfp)

load("D:/SUSHMA DISSERTATION/GBCS.rdata")
ls()
gbcs_data = na.omit(gbcs)


################################################################
###--------------------------AGE-----------------------------###
################################################################
# Fit the fractional polynomial model for age
model_age = mfp(gbcs_data$censrec ~ fp(age, df = 4, select = 0.05), data = gbcs_data)

# Calculate the log-likelihood of the fitted model
logLik(model_age)

# Create a sequence of age values for prediction
age_seq <- seq(min(gbcs_data$age), max(gbcs_data$age), length.out = 650)

# Predict the log(odds) using the fractional polynomial model
predicted_fp <- predict(model_age, newdata = data.frame(age = age_seq), type = "link")

# Plot the predicted log(odds) against age for the fractional polynomial model
plot(age_seq, predicted_fp, type = "l", xlab = "Age", ylab = "Predicted log(Odds)", col = "blue")


################################################################
###-------------------------SIZE-----------------------------###
################################################################
# Fit the fractional polynomial model for size
model_size = mfp(gbcs_data$censrec ~ fp(size, df = 4, select = 0.05), data = gbcs_data)

# Calculate the log-likelihood of the fitted model
logLik(model_size)

# Create a sequence of size values for prediction
size_seq <- seq(min(gbcs_data$size), max(gbcs_data$size), length.out = 650)

# Predict the log(odds) using the fractional polynomial model for size
predicted_fp_size <- predict(model_size, newdata = data.frame(size = size_seq), type = "link")

# Plot the predicted log(odds) against size for the fractional polynomial model
plot(size_seq, predicted_fp_size, type = "l", xlab = "Size", ylab = "Predicted log(Odds)", col = "blue")


################################################################
###------------------------NODES-----------------------------###
################################################################
# Fit the fractional polynomial model for nodes
model_nodes = mfp(gbcs_data$censrec ~ fp(nodes, df = 4, select = 0.05), data = gbcs_data)

# Calculate the log-likelihood of the fitted model
logLik(model_nodes)

# Create a sequence of nodes values for prediction
nodes_seq <- seq(min(gbcs_data$nodes), max(gbcs_data$nodes), length.out = 650)

# Predict the log(odds) using the fractional polynomial model for nodes
predicted_fp_nodes <- predict(model_nodes, newdata = data.frame(nodes = nodes_seq), type = "link")

# Plot the predicted log(odds) against nodes for the fractional polynomial model
plot(nodes_seq, predicted_fp_nodes, type = "l", xlab = "Nodes", ylab = "Predicted log(Odds)", col = "blue")

################################################################
###----------------------PROG_RECP---------------------------###
################################################################
# Fit the fractional polynomial model for prog_recp
model_prog_recp = mfp(gbcs_data$censrec ~ fp(prog_recp, df = 4, select = 0.05), data = gbcs_data)

# Calculate the log-likelihood of the fitted model
logLik(model_prog_recp)

# Create a sequence of prog_recp values for prediction
prog_recp_seq <- seq(min(gbcs_data$prog_recp), max(gbcs_data$prog_recp), length.out = 650)

# Predict the log(odds) using the fractional polynomial model for prog_recp
predicted_fp_prog_recp <- predict(model_prog_recp, newdata = data.frame(prog_recp = prog_recp_seq), type = "link")

# Plot the predicted log(odds) against prog_recp for the fractional polynomial model
plot(prog_recp_seq, predicted_fp_prog_recp, type = "l", xlab = "prog_recp", ylab = "Predicted log(Odds)",col = "blue")

################################################################
###----------------------ESTRG_RECP--------------------------###
################################################################
# Fit the fractional polynomial model for estrg_recp
model_estrg_recp = mfp(gbcs_data$censrec ~ fp(estrg_recp, df = 2, select = 0.05), data = gbcs_data)

# Calculate the log-likelihood of the fitted model
logLik(model_estrg_recp)

# Create a sequence of estrg_recp values for prediction
estrg_recp_seq <- seq(min(gbcs_data$estrg_recp), max(gbcs_data$estrg_recp), length.out = 650)

# Predict the log(odds) using the fractional polynomial model for estrg_recp
predicted_fp_estrg_recp <- predict(model_estrg_recp, newdata = data.frame(estrg_recp = estrg_recp_seq), type = "link")

# Plot the predicted log(odds) against estrg_recp for the fractional polynomial model
plot(estrg_recp_seq, predicted_fp_estrg_recp, type = "l", xlab = "estrg_recp", ylab = "Predicted log(Odds)", col = "blue")

