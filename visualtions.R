library(tidyverse)

df<-read_csv("Admission_Predict.csv")

head(df)

# To find the relationship between GRE Score and TOEFL Score
ggplot(df,aes(x=`GRE Score`,y=`TOEFL Score`,color=`Chance of Admit`))+geom_point()
print("The correlation coefficient between GRE & TOEFL score is") 
cor(df$`GRE Score`,df$`TOEFL Score`)

head(df)

# To find the relationship between Chance of Admit and GRE Score
print("The correlation coefficient between Chance of Admit & GRE Score is")
cor(df$`GRE Score`,df$`Chance of Admit`)
ggplot(df,aes(x=`GRE Score`,y=`Chance of Admit`,color=`TOEFL Score`))+geom_point()

# pie chart for university ratings 
rat1 = 0
rat2 = 0
rat3 = 0
rat4 = 0
rat5 = 0

for (item in df$`University Rating`) {
  if (item==1){
    rat1 = rat1 +1
  }else if (item==2){
    rat2 = rat2 + 1
  }else if (item==3){
    rat3 = rat3 + 1
  }else if (item==4){
    rat4 = rat4 + 1
  }else{
    rat5 = rat5 + 1
  }
}


university_rating <- c('1','2','3','4','5')
rating_freq <- c(rat1,rat2,rat3,rat4,rat5)
rating_freq_count <- data.frame(university_rating,rating_freq)
ggplot(data = rating_freq_count, aes(x = "", y = rating_freq, fill = university_rating),col=c("chartreuse", "blue4")) + 
  geom_bar(stat = "identity") + 
  scale_fill_manual(values=c("#002b80", "#003cb3" ,"#004de6", "#1a66ff", "#4d88ff"))+
  coord_polar("y")


