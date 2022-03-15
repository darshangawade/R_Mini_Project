library(tidyverse)

df<-read_csv("Admission_Predict.csv")


head(df)

# minimum GRE score
min(df$GRE_Score)
# maximum GRE score
max(df$GRE_Score)

# minimum TOEFL score
min(df$TOEFL_Score)
# maximum TOEFL score
max(df$TOEFL_Score)


# Range of university ratings
range(df$University_Rating)
# Range of SOP
range(df$SOP)
# Range of LOR
range(df$LOR)

#mean score of GRE
mean(df$GRE_Score)
#mean score of TOEFL
mean(df$TOEFL_Score)
#mean of CGPA
mean(df$CGPA)

# median of GRE score
median(df$GRE_Score)
# median of TOEFL score
median(df$TOEFL_Score)
# median of CGPA
median(df$CGPA)

# summary of the entire data 
summary(df)

# summary of the data as per the ratings given to university
by(df,df$University_Rating,summary)
# summary of the data as per the SOP
by(df,df$SOP,summary)
# summary of the data as per the LOR
by(df,df$LOR,summary)
