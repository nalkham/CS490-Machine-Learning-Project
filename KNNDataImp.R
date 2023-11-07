library(knitr)
library(tidyverse)
library(broom)
library(naniar)
library(simputation)
library(VIM)

housing_data <- read.csv("~/School Projects/CS499MachineLearningProject/CS499-Machine-Learning-Project/housing.csv")

skimr::skim(housing_data)

KNNData <- kNN(housing_data, variable = c("total_bedrooms"), k = 5)

skimr::skim(KNNData)

write.csv(KNNData, "~/School Projects/CS499MachineLearningProject/CS499-Machine-Learning-Project/housingKNN.csv")