install.packages("mice")
library("mice")

data <-read.csv("housing.csv")

impDr <- mice(data, method = "norm.predict", m = 1)
data_det <- complete(impDr)

impSr <- mice(data, method = "norm.nob", m = 1)
data_sto <- complete(impSr)

write.csv(data_det, 'Deterministic_Regression.csv')
write.csv(data_sto, 'Stochastic_Regression.csv')
