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

attach(housingData)

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


valueModel <- lm(median_house_value ~ median_income + total_rooms + housing_median_age)
summary(valueModel)







