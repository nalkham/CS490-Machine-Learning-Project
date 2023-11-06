options(scipen=999)
housingData <- read.csv("housing.csv", header = T, na.strings = "?", stringsAsFactors = T)

dim(housingData)
names(housingData)
housingData[!complete.cases(housingData), ]

median(housingData$longitude)
median(housingData$latitude)
median(housingData$housing_median_age)
median(housingData$total_rooms)
median(housingData$total_bedrooms)
median(housingData$population)
median(housingData$households)
median(housingData$median_income)
median(housingData$median_house_value)

##total_bedrooms is the only value that has missing data

medBedrooms <- median(housingData$total_bedrooms, na.rm = TRUE)

housingData[is.na(housingData$total_bedrooms), "total_bedrooms"] <- medBedrooms

median(housingData$total_bedrooms)
##calculated median w/o NA's, then replaced NA's with said value
write.csv(housingData, file = "MedianImputedHousingData.csv")

set.seed(1)
splitVal <- sample(c(rep(0,0.7 * nrow(housingData)),
                   rep(1,0.3 * nrow(housingData))))

table(splitVal)
trainingData <- housingData[splitVal ==0, ]
testingData <- housingData[splitVal ==1, ]

head(trainingData)

attach(trainingData)

hist(median_house_value, breaks = 18)

pairs(housingData)
pairs(
  ~ longitude + latitude + housing_median_age + total_rooms + total_bedrooms + population + households + median_income + median_house_value + ocean_proximity,
  data = housingData
)

cor(housingData[, -10])
cor(median_house_value, longitude)
cor(median_house_value, latitude)
cor(median_house_value, housing_median_age)
cor(median_house_value, total_rooms)
cor(median_house_value, total_bedrooms)
cor(median_house_value, population)
cor(median_house_value, households)
cor(median_house_value, median_income)
#cor(median_house_value, ocean_proximity)

## most significant predictor is median_income, followed by total rooms and median age


plot(housing_median_age, median_house_value)
plot(total_rooms, median_house_value)
plot(total_bedrooms, median_house_value)
plot(median_income, median_house_value)

SimpleValueModel <- lm(median_house_value ~ median_income, data = df[trainingData, ])
summary(SimpleValueModel)
abline(SimpleValueModel, col=2, lwd=3)
confint(SimpleValueModel, level=0.99)

valueModel <- lm(median_house_value ~ median_income + total_rooms + housing_median_age + total_bedrooms + households)
summary(valueModel)
abline(valueModel, col=3, lwd=3)
confint(valueModel, level=0.99)

AllDataModel <- lm(median_house_value ~ longitude + latitude + housing_median_age + total_rooms + total_bedrooms + population + households + median_income + ocean_proximity)
summary(AllDataModel)
abline(AllDataModel, col=5, lwd=3)
confint(AllDataModel, level=0.99)


#simple Regression on Test Set
predictedAmountSimple <- predict(SimpleValueModel, testingData)
testingData["Predicted Amount Simple"] <- predictedAmountSimple

plot(testingData$`Predicted Amount Simple`,testingData$median_house_value, xlab = "Actual Amt Spent",
     ylab = "Predicted Amt Spent", main = "Simplistic Model")
abline(lm(testingData$`Predicted Amount Simple` ~ testingData$median_house_value), col = 2)


#Highest R values Regression on Test Set
predictedAmount <- predict(valueModel, testingData)
testingData["Predicted Amount"] <- predictedAmount

plot(testingData$`Predicted Amount`,testingData$median_house_value, xlab = "Actual Amt Spent",
     ylab = "Predicted Amt Spent", main = "Large R-squared Model")
abline(lm(testingData$`Predicted Amount` ~ testingData$median_house_value), col = 2)


#All values Regression on Test Set
ComplexpredictedAmount <- predict(AllDataModel, testingData)
testingData["Complex Predicted Amount"] <- ComplexpredictedAmount

plot(testingData$`Predicted Amount`,testingData$median_house_value, xlab = "Actual Amt Spent",
     ylab = "Predicted Amt Spent", main = "Complex Model")
abline(lm(testingData$`Predicted Amount` ~ testingData$median_house_value), col = 2)

