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
housing <- read.csv("housing.csv") #base data
imput <- hot.deck(housing, m=1, method="best.cell" ,cutoff=10, sdCutoff=1)
imput[["data"]] #data with hot deck imputation
train.size <- trainControl(method = "cv", number = 10) #setting up 10-fold cv

training.samples <- caret::createDataPartition(housing$median_income, p = 0.7, list = FALSE)
train.data <- housing[training.samples, ]
test.data <- housing[-training.samples, ]

model <- lm(median_income ~ poly(median_house_value, 5, raw = TRUE), data = train.data)
predictions <- model %>% predict(test.data)
modelPerformance = data.frame(RMSE = RMSE(predictions, test.data$median_income),
                              R2 = R2(predictions, test.data$median_income))
print(lm(median_income ~ median_house_value + I(median_house_value^2), data = train.data))
print(modelPerformance)

ggplot(train.data, aes(median_house_value, median_income)) +geom_point() +
  stat_smooth(method = lm, formula = y ~ poly(x, 5, raw = TRUE))
