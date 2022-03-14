library(tidyverse)
library(ggcorrplot)

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


# University rating with the variation of average CGPA score
rat1 = 0
rat2 = 0
rat3 = 0
rat4 = 0
rat5 = 0
c1 = 0
c2 = 0
c3 = 0
c4 = 0
c5 = 0
l = as.integer(count(df,'University Rating')) 
print(l[2])
for (i in 1:l[2]) {
  if (df$`University Rating`[i]==1){
    rat1 = rat1 + df$CGPA[i]
    c1 = c1 + 1
  }else if (df$`University Rating`[i]==2){
    rat2 = rat2 + df$CGPA[i]
    c2 = c2 + 1
  }else if (df$`University Rating`[i]==3){
    rat3 = rat3 + df$CGPA[i]
    c3 = c3 + 1
  }else if (df$`University Rating`[i]==4){
    rat4 = rat4 + df$CGPA[i]
    c4 = c4 + 1
  }else{
    rat5 = rat5 + df$CGPA[i]
    c5 = c5 + 1
  }
}

university_rating <- c('1','2','3','4','5')
avg_cgpa <- c(rat1/c1, rat2/c2, rat3/c3, rat4/c4 , rat5/c5)

datas <- data.frame(university_rating,avg_cgpa)
ggplot(data = datas, mapping = aes(x=university_rating, y=avg_cgpa,fill=university_rating)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values=c("#002b80", "#003cb3" ,"#004de6", "#1a66ff", "#4d88ff"))

# variation of chances of admit with CGPA score
cgpa = c(df$CGPA)
chance_of_admit = c(df$`Chance of Admit`)
datas <- data.frame(cgpa,chance_of_admit)

ggplot(datas, aes(x = cgpa, y = chance_of_admit )) +
  geom_line(color = 4,    # Color of the line
            lwd = 1,      # Width of the line
            linetype = 1) # Line type

# how research affects the chances to get admitted
research = c(df$Research)
chance_of_admit = c(df$`Chance of Admit`)
datas <- data.frame(research,chance_of_admit)
print(head(datas))


ggplot(datas, aes(x = research, y = chance_of_admit )) +
  geom_line(color = 4,    # Color of the line
            lwd = 1,      # Width of the line
            linetype = 1) # Line type


# Correlogram to represent the corellation of multiple continuous variables present in the dataset
# Correlation matrix
corr <- round(cor(df), 1)
ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("#002266", "#3377ff", "#b3ccff"), 
           title="Correlogram of Chance of Admit Dataset", 
           ggtheme=theme_bw)


#Bar graph for range of Chance of admit Vs No of students
r1=0
r2=0
r3=0
r4=0
r5=0
for(i in 1:l[2]){
  if(df$`Chance of Admit`[i]>=0 && df$`Chance of Admit`[i]<=0.2){
    r1 = r1+1
  }else if(df$`Chance of Admit`[i]>=0.2 && df$`Chance of Admit`[i]<=0.4){
    r2 = r2+1
  }else if(df$`Chance of Admit`[i]>=0.4 && df$`Chance of Admit`[i]<=0.6){
    r3 = r3+1
  }else if(df$`Chance of Admit`[i]>=0.6 && df$`Chance of Admit`[i]<=0.8){
    r4 = r4+1
  }else{
    r5 = r5+1
  }
}

range <- c('0-0.2', '0.2-0.4', '0.4-0.6', '0.6-0.8', '0.8-1')
no_of_students<- c(r1, r2, r3, r4, r5)
datas <- data.frame(range, no_of_students)
ggplot(data = datas, mapping = aes(x=range, y=no_of_students,fullrange=TRUE)) +
  geom_bar(stat="identity", position = "dodge") +
  scale_fill_manual(values=c("#002b80", "#003cb3" ,"#004de6", "#1a66ff", "#4d88ff"))



