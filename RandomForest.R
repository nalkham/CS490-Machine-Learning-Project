#required for caret package to run
library(lattice)
library(listenv)
#required for caret package to run

library(caret) #used for 10-fold cv with trainControl() and train()
library(randomForest)

#load data
Housing.KNN <- read.csv("housingKNN.csv")
Housing.KNN.edit <- subset(Housing.KNN, select = -c(X,total_bedrooms_imp)) #removes extra columns
Housing.Median <- read.csv("MedianImputedHousingData.csv")
Housing.Median.edit <- subset(Housing.Median, select = -c(X))
Housing.Determ <- read.csv("Deterministic_Regression.csv")
Housing.Determ.edit <- subset(Housing.Determ, select = -c(X))
Housing.Stoch <- read.csv("Stochastic_Regression.csv")
Housing.Stoch.edit <- subset(Housing.Stoch, select = -c(X))
Housing.HD <- read.csv("HotDeck.csv")
Housing.HD.edit <- subset(Housing.HD, select = -c(X))

#used to get a sense of where to put class labels
max(Housing.KNN$median_house_value)
min(Housing.KNN$median_house_value)
mean(Housing.KNN$median_house_value)
median(Housing.KNN$median_house_value)

#divide median housing prices into classes for random forest
Housing.KNN.edit$median_house_value <- cut(Housing.KNN$median_house_value, breaks=c(0, 100000, 200000, 300000, 400000, Inf), 
                                           labels=c("<100K", "100k-200k", "200k-300k", "300k-400k", ">400k"))
table(Housing.KNN.edit$median_house_value)
Housing.Median.edit$median_house_value <- cut(Housing.Median$median_house_value, breaks=c(0, 100000, 200000, 300000, 400000, Inf), 
                                           labels=c("<100K", "100k-200k", "200k-300k", "300k-400k", ">400k"))
table(Housing.Median.edit$median_house_value)
Housing.Determ.edit$median_house_value <- cut(Housing.Determ$median_house_value, breaks=c(0, 100000, 200000, 300000, 400000, Inf), 
                                           labels=c("<100K", "100k-200k", "200k-300k", "300k-400k", ">400k"))
table(Housing.Determ.edit$median_house_value)
Housing.Stoch.edit$median_house_value <- cut(Housing.Stoch$median_house_value, breaks=c(0, 100000, 200000, 300000, 400000, Inf), 
                                           labels=c("<100K", "100k-200k", "200k-300k", "300k-400k", ">400k"))
table(Housing.Stoch.edit$median_house_value)
Housing.HD.edit$median_house_value <- cut(Housing.HD$median_house_value, breaks=c(0, 100000, 200000, 300000, 400000, Inf), 
                                           labels=c("<100K", "100k-200k", "200k-300k", "300k-400k", ">400k"))
table(Housing.HD.edit$median_house_value)

#divide data into training-testing split 70-30
set.seed(513)
train.percent <- 0.7
num.train <- round(nrow(Housing.KNN.edit) * train.percent)
random.indices <- sample(1:nrow(Housing.KNN.edit), num.train)
training.KNN <- Housing.KNN.edit[random.indices, ]
testing.KNN <- Housing.KNN.edit[-random.indices, ]
training.Median <- Housing.Median.edit[random.indices, ]
testing.Median <- Housing.Median.edit[-random.indices, ]
training.Determ <- Housing.Determ.edit[random.indices, ]
testing.Determ <- Housing.Determ.edit[-random.indices, ]
training.Stoch <- Housing.Stoch.edit[random.indices, ]
testing.Stoch <- Housing.Stoch.edit[-random.indices, ]
training.HD <- Housing.HD.edit[random.indices, ]
testing.HD <- Housing.HD.edit[-random.indices, ]

#define train control
train.size <- trainControl(method="cv", number=10, search="random")

#train random forest on knn dataset
set.seed(513)
random.fr.knn <- train(median_house_value~., data=training.KNN, method="rf", metric="Accuracy", trControl=train.size) #with 500 trees
print(random.fr.knn)
set.seed(513)
random.fr.knn.2 <- train(median_house_value~., data=training.KNN, method="rf", ntree=1000, metric="Accuracy", trControl=train.size) #with 1000 trees
print(random.fr.knn.2)

#create model and find testing accuracy
set.seed(513)
KNN.model <- randomForest(formula=median_house_value~., data=training.KNN, ntree=500, mtry=6)
KNN.prediction <- predict(object=KNN.model, newdata=testing.KNN)
confusionMatrix(KNN.prediction, testing.KNN$median_house_value)

#train random forest on hot deck dataset
set.seed(513)
random.fr.HD <- train(median_house_value~., data=training.HD, method="rf", metric="Accuracy", trControl=train.size) #with 500 trees
print(random.fr.HD)
set.seed(513)
HD.model <- randomForest(formula=median_house_value~., data=training.HD, ntree=500, mtry=6)
HD.prediction <- predict(object=HD.model, newdata=testing.HD)
confusionMatrix(HD.prediction, testing.HD$median_house_value)

#train random forest on median dataset
set.seed(513)
random.fr.Median <- train(median_house_value~., data=training.Median, method="rf", metric="Accuracy", trControl=train.size) #with 500 trees
print(random.fr.Median)
set.seed(513)
Median.model <- randomForest(formula=median_house_value~., data=training.Median, ntree=500, mtry=6)
Median.prediction <- predict(object=Median.model, newdata=testing.Median)
confusionMatrix(Median.prediction, testing.Median$median_house_value)

#train random forest on stochastic dataset
set.seed(513)
random.fr.Stoch <- train(median_house_value~., data=training.Stoch, method="rf", metric="Accuracy", trControl=train.size) #with 500 trees
print(random.fr.Stoch)
set.seed(513)
Stoch.model <- randomForest(formula=median_house_value~., data=training.Stoch, ntree=500, mtry=6)
Stoch.prediction <- predict(object=Stoch.model, newdata=testing.Stoch)
confusionMatrix(Stoch.prediction, testing.Stoch$median_house_value)

#train random forest on deterministic dataset
set.seed(513)
random.fr.Determ <- train(median_house_value~., data=training.Determ, method="rf", metric="Accuracy", trControl=train.size) #with 500 trees
print(random.fr.Determ)
set.seed(513)
Determ.model <- randomForest(formula=median_house_value~., data=training.Determ, ntree=500, mtry=6)
Determ.prediction <- predict(object=Determ.model, newdata=testing.Determ)
confusionMatrix(Determ.prediction, testing.Determ$median_house_value)
