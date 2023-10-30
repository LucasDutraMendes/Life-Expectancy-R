##################################################################################
#                                  PCR                                           #
##################################################################################

#Creating the Data Frame to run PCR
DF_PCR <- DF_PCA[,10:13]# PCA Values
DF_PCR <- add_column(DF_PCR, df$Life) #Y value

colnames(DF_PCR)[5] <- "Life" # Life expectancy 

OLS_PCR <- lm(formula = Life ~ ., 
              data = DF_PCR)
summary(OLS_PCR)

lambda_BC1 <- powerTransform(DF_PCR$Life,family="bcPower")
lambda_BC1

DF_PCR_BC <- DF_PCR
DF_PCR_BC$Life <- NULL
DF_PCR_BC$bc_life <- (((DF_PCR$Life ^ lambda_BC1$lambda) - 1) / 
                         lambda_BC1$lambda)

OLS_BC_PCR <- lm(formula = bc_life ~ ., 
                 data = DF_PCR_BC)


sf.test(OLS_PCR$residuals)

summary(OLS_PCR)
summary(OLS_BC_PCR)

sf.test(OLS_PCR$residuals)
sf.test(OLS_BC_PCR$residuals)

ols_vif_tol(OLS_PCR)
ols_vif_tol(OLS_BC_PCR)

ols_test_breusch_pagan(OLS_PCR)
ols_test_breusch_pagan(OLS_BC_PCR)
