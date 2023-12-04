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
write.csv(imput[["data"]][[1]], file="HotDeck.csv")

#deterministic regression imputation linear regression using backwards selection
determ.reg <- read.csv("Deterministic_Regression.csv")
determ.reg.edit <- subset(determ.reg, select = -1) #revomes the first numbered column caused by exporting from R
determ.reg.step <- train(median_house_value ~., data=determ.reg.edit, 
                         method="leapBackward", tuneGrid=data.frame(nvmax = 1:9), 
                         trControl=train.size)
determ.reg.step$results
summary(determ.reg.step$finalModel)
lm(median_house_value ~ longitude + latitude + median_income,
   data=determ.reg.edit)

#stochastic regression imputation linear regression using backwards selection
stoch.reg <- read.csv("Stochastic_Regression.csv")
stoch.reg.edit <- subset(stoch.reg, select = -1)
stoch.reg.step <- train(median_house_value ~., data=stoch.reg.edit, 
                        method="leapBackward", tuneGrid=data.frame(nvmax = 1:9), 
                        trControl=train.size)
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
knn.reg.edit <- subset(knn.reg, select = -c(X,total_bedrooms_imp))
knn.reg.step <- train(median_house_value ~., data=knn.reg.edit, 
                      method="leapBackward", tuneGrid=data.frame(nvmax = 1:9), 
                      trControl=train.size)

knn.reg.step$results
summary(knn.reg.step$finalModel)
lm(median_house_value ~ longitude + latitude + median_income,
   data=knn.reg.edit)
