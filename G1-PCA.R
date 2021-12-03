PAQ2 <- PAQ_Vilma
summary(PAQ2)
require(reshape2)
library(devtools)
gh_install_packages("vqv/ggbiplot")
library(ggbiplot)
library(tidyverse)
library(factoextra)
library(psych)
library("corrplot")


#Cheat sheet 
#1. Statistics make me cry. **
#2. The help files in RStudio are not helpful at all.
#3. I cannot breathe when I see an equation.**
#4. My brain freezes at the mere mention of matrix operations.**
#5. R was designed by aliens to humiliate me.
#6. My friends are better at statistics than me.
#7. R coding makes me weep openly.
#8. There is little or no support for learning R.
#9. If I’m good at statistics people will think I’m a nerd.

str(PAQ2)

#convert to wide format
PAQW2 =dcast(PAQ2, id ~ var, value.var="value")
summary(PAQW2)
save(PAQW2, file = "PAQW.data")

view(PAQW2)
#make id to rowname
rownames(PAQW2) <- PAQW2[,1]
PAQW2[,1] <- NULL

view(PAQW2)

#Decision: exclude sex from data as it is a factor variable
PAQW2$sex <- NULL
head(PAQW2)

#Decision: exclude age from data as it is different scale
PAQW2$age <- NULL
head(PAQW2)

#REMOVE NAs

which(is.na(PAQW2))

which(is.na(PAQW2$Q1_cry)) 
which(is.na(PAQW2$Q8_Support)) 

#decission: change NAs to mean value
PAQW2[93, 1] = 3
PAQW2[232, 8] = 2

sum(is.na(PAQW2))
view(PAQW2)


summary(PAQW2)
#Note: Q8 has a very low median of 1.. Not normal distribution..?  


###
### DATA CLEANING COMPLETE, START ANALYSIS ###
###
cor(PAQW2)
PAQW_cor <- cor(PAQW2)
view(PAQW_cor)

pca_cor <- princomp(PAQW2, cor = TRUE)
summary(pca_cor, loadings=TRUE)

#Finding: Components 1,2 and 3 have eigenvalues larger than 0,7 (std dev) and explain 78% of total variance

#Visualization using factoextra package
fviz_eig(pca_cor)

fviz_pca_ind(pca_cor,
             col.ind = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"))

fviz_pca_var(pca_cor,
             col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE
)
?corrplot
var <- get_pca_var(pca_cor)
corrplot(var$cos2, is.corr = FALSE)

corrplot(var$cos2, is.corr = FALSE)
