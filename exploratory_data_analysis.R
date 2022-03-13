library(tidyverse)

df<-read_csv("Admission_Predict.csv")


head(df)

# minimum GRE score
min(df$`GRE Score`)
# maximum GRE score
max(df$`GRE Score`)

# minimum TOEFL score
min(df$`TOEFL Score`)
# maximum TOEFL score
max(df$`TOEFL Score`)


# Range of university ratings
range(df$`University Rating`)
# Range of SOP
range(df$`SOP`)
# Range of LOR
range(df$`LOR`)

#mean score of GRE
mean(df$`GRE Score`)
#mean score of TOEFL
mean(df$`TOEFL Score`)
#mean of CGPA
mean(df$CGPA)

# median of GRE score
median(df$`GRE Score`)
# median of TOEFL score
median(df$`TOEFL Score`)
# median of CGPA
median(df$CGPA)

# summary of the entire data 
summary(df)

# summary of the data as per the ratings given to university
by(df,df$`University Rating`,summary)
# summary of the data as per the SOP
by(df,df$SOP,summary)
# summary of the data as per the LOR
by(df,df$LOR,summary)
