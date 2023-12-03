library(knitr)
library(tidyverse)
library(broom)
library(naniar)
library(simputation)
library(VIM)
library(rsample)
library(boot)
library(rstanarm)
library(tidymodels)

#Reading The Data Set
housing_data <- read.csv("~/School Projects/CS499MachineLearningProject/CS499-Machine-Learning-Project-1/housing.csv")

#Imputing Missing Data In 'total_bedrooms' variable
housing_data_upd <- kNN(housing_data, variable = c("total_bedrooms"), k = 5)

#Turning total_room and total_bedrooms into a mean value
housing_data_upd$mean_bedrooms = housing_data_upd$total_bedrooms/housing_data_upd$households
housing_data_upd$mean_rooms = housing_data_upd$total_rooms/housing_data_upd$households

drop_vars = c('total_bedrooms', 'total_rooms', 'total_bedrooms_imp')

housing_data_upd = housing_data_upd[ , !(names(housing_data_upd) %in% drop_vars)]
head(housing_data_upd)

#Translating Qualitative Variables Into Boolean Values
classes = unique(housing_data$ocean_proximity)
housing_classes = data.frame(ocean_proximity = housing_data_upd$ocean_proximity)

for (class in classes)
{
  housing_classes[,class] = rep(0, times = nrow(housing_classes))
}

for (i in 1:length(housing_classes$ocean_proximity))
{
  class = as.character(housing_classes$ocean_proximity[i])
  housing_classes[,class][i] = 1
}

class_names = names(housing_classes)
keep_names = class_names[class_names != 'ocean_proximity']
housing_classes = select(housing_classes, one_of(keep_names))

#Scaling Quantitative Variables
non_scaling_variables = c('ocean_proximity', 'median_house_value')
housing_quan = housing_data_upd[, !(names(housing_data_upd) %in% non_scaling_variables)]
housing_quan_scaled = scale(housing_quan)

#Merging All Cleaned Dataframes
housing_data_clean = cbind(housing_classes, housing_quan_scaled, median_house_value = housing_data_upd$median_house_value)

write.csv(housing_data_upd, "~/School Projects/CS499MachineLearningProject/CS499-Machine-Learning-Project/housingKNN.csv")

#Creating Train-Test Split of 70-30
set.seed(1200)
housing_data_split <- initial_split(housing_data_clean, prop = 0.7)
train_data <- training(housing_data_split)
test_data <- testing(housing_data_split)

#Applying Bayesian Regression Model
bayes_model <- stan_glm(median_house_value ~ median_income+mean_rooms+population, data = housing_data_clean, family = "gaussian")
bayes_model2 <-
  linear_reg() %>%
  set_engine("stan", seed = 505) %>%
  fit(median_house_value ~ median_income+mean_rooms+population, data = housing_data_clean)