#install.packages("corrr")
library('corrr')
#install.packages("ggcorrplot")
library(ggcorrplot)
#install.packages("FactoMineR")
library("FactoMineR")
library(factoextra)
library(hot.deck)
library(tidyverse) #from sthda.com tutorial
library(MASS)
#required for caret package to run
library(lattice)
library(listenv)
#required for caret package to run

library(caret) #from sthda.com tutorial
library(leaps) #from sthda.com tutorial

set.seed(513)
housing <- read.csv("Deterministic_Regression.csv")

trimmData <- housing[,2:10]

dataNormal <- scale(trimmData)
corr_matrix <- cor(dataNormal)
ggcorrplot(corr_matrix)

data.pca <- princomp(corr_matrix)
summary(data.pca)
data.pca$loadings[, 1:2]

fviz_eig(data.pca, addlabels = TRUE)
fviz_pca_var(data.pca, col.var = "black")
