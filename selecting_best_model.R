library(tidyverse)
library(caret)
library(party)
library(randomForest)
library(e1071)
library(caTools)


# importing dataset
df<-read_csv("Admission_Predict.csv")
head(df)

# to view dataset -> view(df)
# to get structure of dataset -> str(df)

# splitting dataset in the ratio 8:2
# 8 parts as training data and 2 parts as testing data
splitting <- sample(2,nrow(df), replace = TRUE, prob=c(0.8,0.2))
train <- df[splitting==1,]
test <- df[splitting==2,]


dt_fit <- ctree(Chance_of_Admit ~ ., data = train)
rf_fit <- randomForest(Chance_of_Admit ~ ., data = train)
lg_fit <- glm(Chance_of_Admit ~ ., data = train)
#nb_fit <- naiveBayes(Loan_Status ~ ., data = train)



result1 <- predict(dt_fit,newdata=test,type = "response" )
result2 <- predict(rf_fit,newdata=test,type = "response" )
result3 <- predict(lg_fit,newdata=test,type = "response" )
#result4 <- predict(nb_fit,newdata=test,type = "response" )

#r1 = ifelse(result1> 0.5 , 1, 0)
#r2 = ifelse(result2> 0.5 , 1, 0)
#r3 = ifelse(result3> 0.5 , 1, 0)
#r4 = ifelse(result4> 0.5 , 1, 0)



a <- table(test$Chance_of_Admit,result1)
print(a)
b <- table(test$Chance_of_Admit,result2)
c <- table(test$Chance_of_Admit,result3)
#d <- table(test$Loan_Status,r4)
print("Confusion Matrix for Decision Tree")
confusionMatrix(a)

print("Confusion Matrix for Random forest")
confusionMatrix(b)

print("Confusion Matrix for logistic regression")
confusionMatrix(c)

#confusionMatrix(d)


#saveRDS(rf_fit, "model.rds")



# predicting the chance of admit on the data present in test dadaset


