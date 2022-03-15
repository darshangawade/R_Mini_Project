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

saveRDS(rf_fit, "random_forest_regression_model.rds")

# predicting the chance of admit on the data present in test dadaset
y_pred = predict(rf_fit,test)
# creating a new column in test dataset name as `prediction` and inserting y_pred into it.
test$prediction = y_pred

# input form user
GRE_Score = c(322)
TOEFL_Score = c(110)
University_Rating = c(3)
SOP = c(3.5)
LOR = c(2.5)
CGPA = c(8.67)
Research = c(1)

# creating a dataframe on custom input from user
datas = data.frame(GRE_Score,TOEFL_Score,University_Rating,SOP,LOR,CGPA,Research)

# prediction on custom input
predict(rf_fit,datas)

res = as.numeric(predict(rf_fit,datas)[1])
print(paste('Your chance of addmission is',res*100,'%'))

