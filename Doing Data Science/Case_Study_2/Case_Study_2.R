

library(ggplot2)
library(tidyr)
library(tidyverse)
library(lattice)
library(ggfortify)
library(class)
library(caret)
library(rlang)
library(dplyr)
library(e1071)
library(plotrix)
library(reshape2)
library(gplots)
library(corrplot)
library(naivebayes)
library(ggplot2)
library(tidyr)
library(lattice)
library(ggfortify)




# Read CSV file

emp_data<-read.csv("https://raw.githubusercontent.com/BivinSadler/MSDS-6306-Doing-Data-Science/master/UNIT%2014/CaseStudy2-data.csv",header = TRUE,sep = ",")

#View(emp_data)
str(emp_data)
head(emp_data)
#View(emp_data)
summary(emp_data)
#beerCOTX<-na.omit(beerCOTX)


#Employ Data Male
emp_data_male<-emp_data[emp_data$Gender=="Male",]
#View(emp_data_male)

#Employee Data Female
emp_data_female<-emp_data[emp_data$Gender=="Female",]
#View(emp_data_female)

#Employee Data Attrition Pie Chart
emp_data_att<-emp_data[emp_data$Attrition=="Yes",]
#View(emp_data_att)

att_df<-data.frame(emp_data$Attrition)
summary(att_df)
#create pie chart
qty <- c(140,730)
att <- c("YES", "NO")
pct <- round(qty/sum(qty)*100)
att <- paste(att, pct) # add percents to labels 
att <- paste(att,"%",sep=" ") # ad % to labels 
pie3D(qty,labels = att,explode=0.3,
    main="Pie Chart of Attrition") 

#emp_data_att <- melt(emp_data_att, id.vars="Attrition")

#ggplot(emp_data_att, aes(Attrition,value)) + geom_point() + stat_smooth() + facet_wrap(~variable)









# Sum of field
#sum(DF[which(DF[,1]>30 & DF[,4]>90),2])
#att_no<-sum(emp_data[which(emp_data[,3]=="No"),2])
#att_no






#Stack chart Education

ggplot(emp_data, aes(x = Education, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  
  labs(title = "Attrition Vs Education", x = "Education", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart Age
ggplot(emp_data, aes(x = Age, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs Age", x = "Age", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart BusinessTravel
ggplot(emp_data, aes(x = BusinessTravel, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs BusinessTravel", x = "BusinessTravel", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart EducationField
ggplot(emp_data, aes(x = EducationField, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs EducationFieldl", x = "EducationField", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()


#Stack chart JobInvolvement
ggplot(emp_data, aes(x = JobInvolvement, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs JobInvolvement", x = "JobInvolvement", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()


#Stack chart  JobLevel
ggplot(emp_data, aes(x =  JobLevel, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs  JobLevel", x = " JobLevel", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()


#Stack chart  JobRole
ggplot(emp_data, aes(x =  JobRole, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs  JobRole", x = " JobRole", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()


#Stack chart   JobSatisfaction
ggplot(emp_data, aes(x =   JobSatisfaction, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs   JobSatisfaction", x = "  JobSatisfaction", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart    MaritalStatus
ggplot(emp_data, aes(x =    MaritalStatus, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    MaritalStatus", x = "   MaritalStatus", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart    MonthlyIncome
ggplot(emp_data, aes(x =    MonthlyIncome, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    MonthlyIncome", x = "   MonthlyIncome", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart   NumCompaniesWorked
ggplot(emp_data, aes(x =   NumCompaniesWorked, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs   NumCompaniesWorked", x = "  NumCompaniesWorked", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart   Over18
ggplot(emp_data, aes(x =   Over18, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs   Over18", x = "  Over18", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()


#Stack chart   Over18
ggplot(emp_data, aes(x =    OverTime, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    OverTime", x = "   OverTime", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart   Over18
ggplot(emp_data, aes(x =    OverTime, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    OverTime", x = "   OverTime", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()


#Stack chart    OverTime
ggplot(emp_data, aes(x =    OverTime, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    OverTime", x = "   OverTime", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()



#Stack chart    RelationshipSatisfaction
ggplot(emp_data, aes(x =    RelationshipSatisfaction, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    RelationshipSatisfaction", x = "   RelationshipSatisfaction", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart    StockOptionLeveln
ggplot(emp_data, aes(x =StockOptionLevel, y =Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    StockOptionLeveln", x = "StockOptionLeveln", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart    TotalWorkingYears
ggplot(emp_data, aes(x =    TotalWorkingYears, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    TotalWorkingYears", x = "   TotalWorkingYears", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart   YearsAtCompany
ggplot(emp_data, aes(x =   YearsAtCompany, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs   YearsAtCompany", x = "  YearsAtCompany", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart   YearsInCurrentRole
ggplot(emp_data, aes(x =   YearsInCurrentRole, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs   YearsInCurrentRole", x = "  YearsInCurrentRole", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart   YearsSinceLastPromotion
ggplot(emp_data, aes(x =   YearsSinceLastPromotion, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs   YearsSinceLastPromotion", x = "  YearsSinceLastPromotion", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()

#Stack chart    YearsWithCurrManager
ggplot(emp_data, aes(x =    YearsWithCurrManager, y = Attrition, fill =Attrition )) + 
  geom_bar(stat = "identity") +
  labs(title = "Attrition Vs    YearsWithCurrManager", x = "   YearsWithCurrManager", y = "Attrition")+
  #guides(fill = FALSE) +
  theme_bw()












#Plot of Attrion on income vs age 
plot(emp_data$Age,emp_data$MonthlyIncome,main="Sal vs Age with ATT", col="gray",pch=15)
points(emp_data_att$Age, emp_data_att$MonthlyIncome,col="red",pch=15)
legend("topleft", inset=.02, title="Emp Comp Att",
       c("No", "Yes"), fill=c("gray", "red"), horiz=TRUE, cex=0.8)



#Plot of Attrion on income vs Education
plot(emp_data$Education,emp_data$MonthlyIncome,main="Sal vs Education with ATT", col="gray",pch=15)
points(emp_data_att$Education, emp_data_att$MonthlyIncome,col="red",pch=15)
legend("topleft", inset=.02, title="Emp Comp Att",
       c("No", "Yes"), fill=c("gray", "red"), horiz=TRUE, cex=0.8)





# order the data in assending order of monthly salary 

emp_data<-emp_data[order(emp_data$MonthlyIncome),]

# split data into traing and test set in 80/20 ratio

#count number of rows in emp_data table
set.seed(8)
r_emp_data<-nrow(emp_data)
r_emp_data
#take rundom sample of 80% of rows
n_emp<-sample(r_emp_data,0.8*r_emp_data)
n_emp


#Create Training set for employe data
trg_emp<-emp_data[n_emp,]
#View(trg_emp)
str(trg_emp)
summary(trg_emp)
#Create test set for employee data
tst_emp<-emp_data[-n_emp,]
#View(tst_emp)
str(tst_emp)
summary(tst_emp)

#Built a linear model.extract

str(emp_data)
trg_att_prb<-naiveBayes(trg_emp$Attrition ~ Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + Over18 + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StandardHours + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data=trg_emp)    
#trg_att_prb
#Compute the prediction accurcy on test set
pred<-predict(trg_att_prb,tst_emp)

mean(pred==tst_emp$Attrition)


trg_att_prb2<-naive_bayes(trg_emp$Attrition ~ Age + BusinessTravel + DailyRate + Department + DistanceFromHome + Education + EducationField + EnvironmentSatisfaction + Gender + HourlyRate + JobInvolvement + JobLevel + JobRole + JobSatisfaction + MaritalStatus + MonthlyIncome + MonthlyRate + NumCompaniesWorked + Over18 + OverTime + PercentSalaryHike + PerformanceRating + RelationshipSatisfaction + StandardHours + StockOptionLevel + TotalWorkingYears + TrainingTimesLastYear + WorkLifeBalance + YearsAtCompany + YearsInCurrentRole + YearsSinceLastPromotion + YearsWithCurrManager, data=trg_emp)    
#trg_att_prb
pred2<-predict(trg_att_prb2,tst_emp)

mean(pred2==tst_emp$Attrition)
