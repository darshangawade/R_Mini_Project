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


rf_fit <- randomForest(Chance_of_Admit ~ ., data=df, ntree=1000,
                       keep.forest=FALSE, importance=TRUE)
`OOB Sample error` = rf_fit

# predicting the chance of admit on the data present in test dadaset
y_pred = predict(rf_fit,test)
# creating a new column in test dataset name as `prediction` and inserting y_pred into it.
test$prediction = y_pred

print(rf_fit)
plot(`OOB Sample error`)
summary(rf_fit)

ImpData <- as.data.frame(importance(rf.fit))
ImpData$Var_Name <- row.names(ImpData)

ggplot(ImpData, aes(x=Var_Name, y=`%IncMSE`)) +
  geom_segment( aes(x=Var_Name, xend=Var_Name, y=0, yend=`%IncMSE`), color="skyblue") +
  geom_point(aes(size = IncNodePurity), color="blue", alpha=0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position="bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )