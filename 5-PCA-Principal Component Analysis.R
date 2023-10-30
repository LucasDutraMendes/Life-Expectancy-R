##################################################################################
#                                 PACKAGES USED                                  #
##################################################################################

packs <- c("plotly","tidyverse","knitr","kableExtra","car","rgl","gridExtra",
           "PerformanceAnalytics","reshape2","rayshader","psych","pracma",
           "polynom","rqPen","ggrepel","factoextra","sp","tmap","magick")

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
#                                  PCA                                           #
##################################################################################

DF_PCA <- df[c("Position","City","Sun", "Water","Obesity",
                  "Pollution","HoursWorked","Happiness","Activities",
                  "Restaurants","Gym")]


rownames(DF_PCA) <- DF_PCA[,2]

#creating new DF to put the attributes at the same scale
DF_PCA_SCALE <- DF_PCA 

DF_PCA <- DF_PCA[, -1:-2]

rownames(DF_PCA) <- DF_PCA_SCALE[,2] 
DF_PCA_SCALE$Position <- NULL 
DF_PCA_SCALE$City <- NULL 
# scaling the attributes 
DF_PCA_SCALE <- scale(DF_PCA_SCALE)

#Creating Correlation Matrix
rho <- cor(DF_PCA)

#Bartlett's test that a correlation matrix is an identity matrix
#p-value 1.283726e-88 - validates the usage of PCA model
cortest.bartlett(R = rho)

#KMO test
KMO(r = rho)

#PCA Model
afpc <- prcomp(DF_PCA_SCALE, center = TRUE)

#Summary
#we are keeping components greater than 1,
#capturing 86.29% of the total base variance for this model.
summary(afpc)

#PCA components
afpc$sdev^2
afpc$rotation
afpc$center
afpc$scale
afpc$x

#Visualizing the weights that each variable has in each principal component 
#obtained by PCA
ggplotly(
  data.frame(afpc$rotation[,1:4]) %>%
    mutate(var = names(DF_PCA)) %>%
    melt(id.vars = "var") %>%
    mutate(var = factor(var)) %>%
    ggplot(aes(x = var, y = value, fill = var)) +
    geom_bar(stat = "identity", color = "black") +
    facet_wrap(~variable) +
    labs(x = NULL, y = NULL, fill = "Legenda:") +
    scale_fill_viridis_d() +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 90)))

# % of explained variances 
ggplotly(
  fviz_eig(X = afpc,
           ggtheme = theme_bw(), 
           barcolor = "black", 
           barfill = "dodgerblue4",
           linecolor = "darkgoldenrod3"))

#factor loading
k <- sum((afpc$sdev ^ 2) > 1) 
factor_loading <- afpc$rotation[, 1:k] %*% diag(afpc$sdev[1:k])

#visualizing them
data.frame(factor_loading) %>%
  rename(F1 = X1,
         F2 = X2,
         F3 = X3,
         F4 = X4) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)  

#Visualizing Communalities = refers to the total shared variance
# with all factors
data.frame(rowSums(factor_loading ^ 2)) %>%
  rename(comunalidades = 1) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

factors_scores <- t(afpc$rotation)/afpc$sdev 
colnames(factors_scores) <- colnames(DF_PCA_SCALE)

score_D1 <- factors_scores[1,]
score_D2 <- factors_scores[2,]
score_D3 <- factors_scores[3,]
score_D4 <- factors_scores[4,]

F1 <- t(apply(DF_PCA_SCALE, 1, function(x) x * score_D1))
F2 <- t(apply(DF_PCA_SCALE, 1, function(x) x * score_D2))
F3 <- t(apply(DF_PCA_SCALE, 1, function(x) x * score_D3))
F4 <- t(apply(DF_PCA_SCALE, 1, function(x) x * score_D4))

F1 <- data.frame(F1) %>%
  mutate(fator1 = rowSums(.) * 1)

F2 <- data.frame(F2) %>%
  mutate(fator2 = rowSums(.) * -1)

F3 <- data.frame(F3) %>%
  mutate(fator3 = rowSums(.) * 1)

F4 <- data.frame(F4) %>%
  mutate(fator4 = rowSums(.) * 1)

#creating the components based on the data analysis.

DF_PCA["Daily_Overload"] <- F1$fator1
DF_PCA["Leisure"] <- F2$fator2
DF_PCA["Sedentarism"] <- F3$fator3
DF_PCA["HealthyHabits"] <- F4$fator4

shared_variance <- (afpc$sdev ^ 2/sum(afpc$sdev ^ 2))

view(shared_variance)
view(DF_PCA)

cor(DF_PCA, use = "all.obs",
    method = "pearson")

#inter-relationship between variables 
DF_PCA %>%
  correlation(method = "pearson",) %>%
  plot()
