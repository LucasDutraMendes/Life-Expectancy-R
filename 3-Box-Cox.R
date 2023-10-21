
##################################################################################
#                              NONLINEAR REGRESSION BOX-COX                      #
##################################################################################
NONLINEAR REGRESSION BOX-COX 
# Lambda Box-Cox using bcPower also can use -> "yjPower bcnPower"
lambda_BC <- powerTransform(DF_MLR$Life,family="bcPower") # powerTransform package car#
lambda_BC # result 11.72205 

# BOX-COX values applied to Life attribute
DF_MLR$BC_Life <- (((DF_MLR$Life ^ lambda_BC$lambda) - 1) / 
                     lambda_BC$lambda)

#Box-Cox Model -> Lambda applied to var Life as our Y
MLR_BC_Life <- lm(formula = BC_Life ~ . - Life, 
                  data = DF_MLR)

#Comparing MLR against Box-Cox
#Watch out!!! PARAMETERS ARE NOT DIRECTLY COMPARABLE!
export_summs(MLR_Life, MLR_BC_Life, scale = F, digits = 4, 
             model.names = c("MLR-Linear", "BOX-COX"))

summary(MLR_BC_Life) # insignificant improvement in the adjusted R²,
#only two statistically significant variables

#Shapiro-Francia test
sf.test(MLR_BC_Life$residuals) #Excellent result after Box-Cox transformation
# p-value = 0.6947 

