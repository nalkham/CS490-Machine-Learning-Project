library(datasets) #for Train-Test split
library(boot) #for cv.glm()
library(stats) #for glm()

#ALWAYS USE THIS BEFORE DOING ANYTHING
set.seed(123)
#ALWAYS

housingKNN <- read.csv("housingKNN.csv")
housingKNN.edit <- subset(housingKNN, select = -c(X,total_bedrooms_imp, ocean_proximity)) #removes extra columns

#divide data into training and testing sets in a 70-30 split (RESET SEED)
train.percent <- 0.7
num.train <- round(nrow(housingKNN.edit) * train.percent)
random.indices <- sample(1:nrow(housingKNN.edit), num.train)
training <- housingKNN.edit[random.indices, ]
testing <- housingKNN.edit[-random.indices, ]

#order of variables to be backwards selected for KNN is derived from HotDeckImputation.R
#in order of removal: households, total_rooms, housing_median_age,
#population, total_bedrooms, longitude, latitude (ocean proximity removed since it is
# not contiguous data).
# ocean_proximity excluded for not being contiguous
set.seed(123)
formula9 = median_house_value ~ median_income + latitude + longitude + total_bedrooms + population + housing_median_age + total_rooms + households
glm.9 <- glm(formula=formula9, family="poisson", data=training)
cv.glm(data=training, glmfit= glm.9, K=10)

set.seed(123)
formula8 = median_house_value ~ median_income + latitude + longitude + total_bedrooms + population + housing_median_age + total_rooms
glm.8 <- glm(formula=formula8, family="poisson", data=training)
cv.glm(data=training, glmfit= glm.8, K=10)

set.seed(123)
formula7 = median_house_value ~ median_income + latitude + longitude + total_bedrooms + population + housing_median_age
glm.7 <- glm(formula=formula7, family="poisson", data=training)
cv.glm(data=training, glmfit= glm.7, K=10)

set.seed(123)
formula6 = median_house_value ~ median_income + latitude + longitude + total_bedrooms + population
glm.6 <- glm(formula=formula6, family="poisson", data=training)
cv.glm(data=training, glmfit= glm.6, K=10)

set.seed(123)
formula5 = median_house_value ~ median_income + latitude + longitude + total_bedrooms
glm.5 <- glm(formula=formula5, family="poisson", data=training)
cv.glm(data=training, glmfit= glm.5, K=10)

set.seed(123)
formula4 = median_house_value ~ median_income + latitude + longitude
glm.4 <- glm(formula=formula4, family="poisson", data=training)
cv.glm(data=training, glmfit= glm.4, K=10)

set.seed(123)
formula3 = median_house_value ~ median_income + latitude
glm.3 <- glm(formula=formula3, family="poisson", data=training)
cv.glm(data=training, glmfit= glm.3, K=10)

set.seed(123)
formula2 = median_house_value ~ median_income
glm.2 <- glm(formula=formula2, family="poisson", data=training)
cv.glm(data=training, glmfit= glm.2, K=10)

poisson.glm <- glm.4 #the best model to go with
summary(glm.4)
predicted <- predict(glm.4, testing, interval= "prediction")
sqrt(mean(testing$median_house_value - predicted)^2)
mean(testing$median_house_value)
