###--------------German Breast Cancer Study------------------###


# Load the required packages
library(mfp)

load("D:/SUSHMA DISSERTATION/GBCS.rdata")
ls()
gbcs_data = na.omit(gbcs)


# Fit the fractional polynomial model for all the variables
model_full <- mfp(gbcs_data$censrec ~ fp(age, df = 4, select = 0.05) + fp(size, df = 4, select = 0.05)
                  + fp(nodes, df = 4, select = 0.05) +fp(prog_recp, df = 4, select = 0.05)
                  + fp(estrg_recp, df = 4, select = 0.05),data = gbcs_data)
