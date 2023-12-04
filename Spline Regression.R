library(hot.deck)
library(tidyverse) #from sthda.com tutorial
library(MASS)

#required for caret package to run
library(lattice)
library(listenv)
#required for caret package to run

library(caret) #from sthda.com tutorial
library(leaps) #from sthda.com tutorial
library(splines) #for spline regression
library(ISLR)      
library(dplyr)     
library(ggplot2)   
install.packages('earth')
library(earth)

set.seed(513)
housing <- read.csv("housing.csv") #base data
imput <- hot.deck(housing, m=1, method="best.cell" ,cutoff=10, sdCutoff=1)
imput[["data"]] #data with hot deck imputation
train.size <- trainControl(method = "cv", number = 10) #setting up 10-fold cv

#base data linear regression using backwards selection, with all obervations with missing values removed
base.step <- train(median_house_value ~., data=na.omit(housing), method="leapBackward",
                   tuneGrid=data.frame(nvmax = 1:9), trControl=train.size)
base.step$results
summary(base.step$finalModel)
lm(median_house_value ~ longitude + latitude + median_income,
   data=housing) #preductors recieved from finalModel

#hot deck linear regression using backwards selection
hot.step <- train(median_house_value ~., data=imput[["data"]][[1]], method="leapBackward",
                  tuneGrid=data.frame(nvmax = 1:9), trControl=train.size)
hot.step$results
summary(hot.step$finalModel)
lm(median_house_value ~ longitude + latitude + median_income,
   data=imput[["data"]][[1]])
write.matrix(imput[["data"]][[1]], file="HotDeck.csv")

stoch.reg.step$results
summary(stoch.reg.step$finalModel)
lm(median_house_value ~ longitude + latitude + median_income,
   data=stoch.reg.edit)

#median value imputation linear regression using backwards selection
med.reg <- read.csv("MedianImputedHousingData.csv")
med.reg.edit <- subset(med.reg, select = -1)
med.reg.step <- train(median_house_value ~., data=med.reg.edit, 
                        method="leapBackward", tuneGrid=data.frame(nvmax = 1:9), 
                        trControl=train.size)
med.reg.step$results
summary(med.reg.step$finalModel)
lm(median_house_value ~ longitude + latitude + median_income,
   data=med.reg.edit)

#KNN imputation linear regression using backwards selection
knn.reg <- read.csv("housingKNN.csv")
knn.reg.edit <- subset(knn.reg, select = -1)
knn.reg.step <- train(median_house_value ~., data=knn.reg.edit, 
                      method="leapBackward", tuneGrid=data.frame(nvmax = 1:9), 
                      trControl=train.size)

knn.reg.step$results
summary(knn.reg.step$finalModel)
lm(median_house_value ~ longitude + latitude + median_income,
   data=knn.reg.edit)


#spline regression model code attempt 1
#training.samples <-med.reg.edit$median_house_value %>%
#  createDataPartition(p = 0.9, list = FALSE)
#spline_train.data <- med.reg.edit[training.samples,]
#spline_test.data <- med.reg.edit[-training.samples,]

#knots <- quantile(spline_train.data$median_income, p = c(0.25, 0.5, 0.75))
#model <- lm (median_house_value ~ bs(median_income, knots = knots), data = spline_train.data)
#predictions <- model %>% predict(spline_test.data)
#data.frame(
#  RMSE = RMSE(predictions, spline_test.data$median_house_value),
#  R2 = R2(predictions, spline_test.data$median_house_value)
#)

#ggplot(spline_test.data, aes(median_income, median_house_value) ) +
#  geom_point() +
#  stat_smooth(method = lm, formula = y ~ splines::bs(x, df = 3))

#spline Regression attempt 2

hyper_grid <- expand.grid(degree = 3:7,
                          nprune = seq(2, 50, length.out = 10) %>%
                            floor())
#fit MARS model using cross-validation
cv_mars <- train(
  median_house_value ~., data=na.omit(housing),
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid)

#store the best tuned model according to RMSE
cv_mars$results %>%
  filter(nprune==cv_mars$bestTune$nprune, degree ==cv_mars$bestTune$degree)

print(cv_mars)

ggplot(cv_mars)

train(median_house_value ~., data=na.omit(housing), method="leapBackward",
      tuneGrid=data.frame(nvmax = 1:9), trControl=train.size)
