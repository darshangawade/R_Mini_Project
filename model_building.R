library(tidyverse)
library(randomForest)
library(caret)

# importing dataset
df<-read_csv("Admission_Predict.csv")

# to view dataset -> view(df)
# to get structure of dataset -> str(df)

# splitting dataset in the ratio 7:3
# 7 parts as training data and 3 parts as testing data
splitting <- sample(2,nrow(df), replace = TRUE, prob=c(0.7,0.3))
train <- df[splitting==1,]
test <- df[splitting==2,]


rf_fit <- randomForest(Chance_of_Admit ~ ., data=train)

saveRDS(rf_fit, "model.rds")

# predicting the chance of admit on the data present in test dadaset


