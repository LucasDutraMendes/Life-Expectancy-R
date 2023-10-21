##################################################################################
#                                 PACKAGES USED                                  #
#                               UNTIL THE END OF THE STUDY                       #
##################################################################################

packs <- c("dplyr","tidyverse","cluster","dendextend","factoextra","fpc",
             "gridExtra","readxl",'readr',"plotly","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp","rgl","GGally")

if(sum(as.numeric(!packs %in% installed.packages())) != 0){
  installer <- packs[!packs %in% installed.packages()]
  for(i in 1:length(installer)) {
    install.packages(installer, dependencies = T)
    break()}
  sapply(packs, require, character = T) 
} else {
  sapply(packs, require, character = T) 
}
##################################################################################
#                                  Dataset                                       #
##################################################################################

#creating new DF for working with OLS model
DF_MLR <- df[c("Position","City","Sun", "Water", "Obesity","Life",
                     "Pollution","HoursWorked","Happiness","Activities",
                     "Restaurants","Gym")]

#removing Position & City attributes.
DF_MLR <- df[, -1:-2]

head(DF_MLR, 5)

##################################################################################
#                               CORRELATION                                      #
##################################################################################


# Understanding the correlation among the attributes with the lines below
# as Life is the attribute to be understand, it's being removed from the 
# correlation map, in which shows values above 0.5 to several attributes
# which may indicate multicollinearity
DF_MLR_Life <- DF_MLR[, -4]
ggcorr(DF_MLR_Life, label=T)

cor(DF_MLR_Life, use = "all.obs",
        method = "pearson")

#inter-relation among the attributes  
DF_MLR_Life %>%
  correlation(method = "pearson",) %>%
  plot()

cor(DF_MLR$Life, DF_MLR$Happiness) # = 0.7245871, multicollinearity?
cor(DF_MLR$Water, DF_MLR$Happiness) # = 0.8128765, multicollinearity?

#chart correlation
chart.Correlation((DF_MLR_Life), histogram = TRUE)

##################################################################################
#                       Multiple Linear Regression (MLR)                         #
##################################################################################

# MLR
MLR_Life <- lm(formula = Life ~ ., 
                      data = DF_MLR)

# summary shows R-square .7, p-value of the F-test distribution 
# is statistically different from 0 at a 95% confidence level, 
# and is therefore statistically significant for predicting 
# the phenomenon studied. Among the variables studied, 
# only the intercept, HoursWorked, Happiness 
# and Gym were statistically significant. 
summary(MLR_Life)
summ(MLR_Life, confint = T, digits = 3, ci.width = .95)
export_summs(MLR_Life, scale = F, digits = 5)

#Confidence Intervals
confint(MLR_Life, level = 0.95) # significance 5%

#aggregate
aggregate(DF_MLR$Happiness ~ DF_MLR$Life, FUN = mean)

##################################################################################
#            SHAPIRO-FRANCIA NORMALITY TEST                                      #
##################################################################################

# Shapiro-Francia normality test
# p-value < 0.05 indicating that the distribution of the data 
# does not follow a normal distribution
sf.test(MLR_Life$residuals) # result p-value = 0.005673


#Shapiro-Wilk Normality Test test not recommended to this dataset due to its size
shapiro.test(MLR_Life$residuals) # result not considered p-value = 0.008021
