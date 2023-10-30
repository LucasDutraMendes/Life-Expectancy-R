##################################################################################
#                               STEPWISE                                         #
##################################################################################

###### Stepwise can resolve unnormal distribution
##### Stepwise can resolve Collinearity problems - even with
## populations -> Even for populations it is possible to implement stepwise
# to eliminate P>value above 0.05 so as not to generate biases in the betas due to
# very high Collinearity identified by high VIF

#STEPWISE
step_MLR <- step(MLR_Life, k = 3.841459) # OLS/MLR model
step_bc <- step(MLR_BC_Life, k = 3.841459 ) # Box-Cox Model

summary(step_MLR)#checking the summary - only 2 attributes passed
summary(step_bc) #checking the summary - only 2 attributes passed

#Shapiro-Francia test
sf.test(step_MLR$residuals) # OLS/MLR model
sf.test(step_bc$residuals) # Box-Cox Model
#Box-Cox Model - the data now follows a normal distribution p-value = .0795

##################################################################################
#                              VIF - Collinearity test                           #
##################################################################################

#Collinearity  = high relation between among variables X
#VIF = Variance Inflation Factor = 1 / Tolerance -> between 1 to +infinite
#Tolerance = 1 - R² -> between 1 a 0

# VIF test is the same for OLS/MLR and Box-Cox, since we've transformed only Y
# VIF is different for stepwise models since we have less X variables
# VIF above 4 may indicate Collinearity 
ols_vif_tol(MLR_Life) # OLS/MLR model
ols_vif_tol(MLR_BC_Life) #Box-Cox Model

ols_vif_tol(step_MLR) # Stepwise OLS/MLR model
ols_vif_tol(step_bc) # Stepwise Box-Cox Model

##################################################################################
#                              Heteroscedasticity                                #
##################################################################################

##### Heteroscedasticity ###### = correlation between
#X variables + error terms
#H0 of the test: absence of heteroscedasticity. > 0.05
#H1 of the test: heteroscedasticity <0.5
#i.e. correlation between residuals and one or more explanatory variables, 
#which indicate the omission of relevant variable!
#Breusch-Pagan test for diagnosing heteroscedasticity

ols_test_breusch_pagan(MLR_Life) # OLS/MLR model - Chi2 = 4.581403e-05
ols_test_breusch_pagan(MLR_BC_Life) #Box-Cox Model - Chi2 = 0.8195416 

ols_test_breusch_pagan(step_MLR)
ols_test_breusch_pagan(step_bc)

#Conclusion - BoxCox resolves Heterocedasticity problem, but the model
# still has Collinearity issues.
# the question is, how to resolve it and obtain valuable insights? 
# We want to understand the factors for a long life expectancy. 
