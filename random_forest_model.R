library(tidyverse)
library(randomForest)
df<-read_csv("Admission_Predict.csv")

splitting <- sample(nrow(df), 0.7*nrow(df), replace = FALSE)
train <- df[splitting,]
test <- df[-splitting,]
print(train[-9])
# Fitting Random Forest to the train dataset
set.seed(120)  # Setting seed
classifier_RF = randomForest(x = train[-9],
                             y = train$`Chance of Admit`,
                             ntree = 500)

classifier_RF

available_test =test[-9]

# Predicting the Test set results
y_pred = predict(classifier_RF, newdata = test[-9])
y_pred = c(y_pred)

print(y_pred)
print(test[, 9])
# Confusion Matrix
confusion_mtx = table(test[,9], y_pred)
confusion_mtx

# Plotting model
plot(classifier_RF)

# Importance plot
importance(classifier_RF)

# Variable importance plot
varImpPlot(classifier_RF)





