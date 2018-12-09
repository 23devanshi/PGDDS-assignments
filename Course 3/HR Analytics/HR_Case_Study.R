################## HR ANALYTICS CASE STUDY ###################
################################################################
#Business Understanding
#Data Understanding
#Data Preparation & EDA
#Model Building 
#Model Evaluation
################################################################

### Business Understanding:

# Historically 15% of the workforce leaves the company and the management wants to curb this or bring down the 
# attrition level, by focusing on the factors that influence an employee to leave. Basis the factors identified
# the management wants to make changes to the their workplace, in order to get most of their employees to stay. 
# Also, they want to know which of these variables is most important and needs to be addressed right away

## AIM:

# The aim is to model the probability of attrition 
# and help the company identify which factors influence an employee to leave so that
# the company can make the requisite changes and in effect, motivate the employees to stay.
# Whether an employee will move out or not will depend on data from the following five buckets:

# 1. General Employee Information
# 2. Employee survey data
# 3. Manager survey data
# 4. in-time
# 5. out-time

############## Install and Load the required packages##############################################
#install.packages("MASS")
#install.packages("dplyr")
#install.packages("car")
#install.packages("e1071")
#install.packages("caret", dependencies = c("Depends", "Suggests"))
#install.packages("ggplot2")
#install.packages("cowplot")
#install.packages("caTools")
#install.packages("GGally")
#install.packages("ROCR")
#install.packages("corrplot")
#install.packages("ModelMetrics")
#install.packages("reshape")

#setwd("D:/PA-I_Case_Study_HR_Analytics")

library(MASS)
library(dplyr)
library(car)
library(e1071)
library(caret)
library(ggplot2)
library(cowplot)
library(caTools)
library(GGally)
library(ROCR)
library(corrplot)
library(ModelMetrics)

####### Understanding the data provided ###############################################

#Extracting the data

main <- read.csv("general_data.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
employee <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
manager <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE, na.strings = c("",NA))

str(employee) # 4410 obs. of  4 variables
str(manager) # 4410 obs. of  3 variables
str(main) # 4410 obs. of  24 variables including the target variable attrition indicating whether the employee has left in the previous year or not
str(in_time)   # 4410 obs. of  262 variables
str(out_time)  # 4410 obs. of  262 variables

#change 1st column's column name in in_time and out_time to "EmployeeID"

colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

#Check if there are duplicates
sum(duplicated(main$EmployeeID))

# Collating the data together in one single file
length(unique(tolower(employee$EmployeeID)))      # 4410, confirming EmployeeID is key 
length(unique(tolower(manager$EmployeeID)))       # 4410, confirming EmployeeID is key
length(unique(tolower(main$EmployeeID)))          # 4410, confirming EmployeeID is key
length(unique(tolower(in_time$EmployeeID)))       # 4410, confirming EmployeeID is key
length(unique(tolower(out_time$EmployeeID)))      # 4410, confirming EmployeeID is key

#Check if EmployeeID is same in datasets to be merged
setdiff(employee$EmployeeID,manager$EmployeeID)   # Identical EmployeeID across these datasets
setdiff(employee$EmployeeID,main$EmployeeID)      # Identical EmployeeID across these datasets
setdiff(employee$EmployeeID,in_time$EmployeeID)   # Identical EmployeeID across these datasets
setdiff(employee$EmployeeID,out_time$EmployeeID)  # Identical EmployeeID across these datasets

#Merging the required datasets
main <- merge(main, employee, by = "EmployeeID")
main <- merge(main, manager, by = "EmployeeID")

str(main)

#Checking for NA values
sapply(main, function(x) length(which(is.na(x))))


#Convert discrete variables into factor type

main$BusinessTravel <- as.factor(main$BusinessTravel)
main$JobRole <- as.factor(main$JobRole)
main$Department <- as.factor(main$Department)
main$MaritalStatus <- as.factor(main$MaritalStatus)
main$EducationField <- as.factor(main$EducationField)
main$Gender <- as.factor(main$Gender)

#Dropping unnecessary variables

main$Over18 <- NULL #all values are same
main$StandardHours <- NULL #all values are same
main$EmployeeCount <- NULL #all values are same

summary(main)

#######  Checking for outliers ###############

#Outliers in MonthlyIncome
quantile(main$MonthlyIncome, probs = seq(0,1,0.01))
# capping till 2% quantile
main$MonthlyIncome <- sapply(main$MonthlyIncome, function(x) ifelse(x < 18590.0 ,18590.0,x))


######## Deriving variables for analysis ###########

#Convert dates into proper format in in_time

for (i in 2:ncol(in_time))
{
  in_time[,i] <- as.POSIXct(in_time[,i], format = "%Y-%m-%d %H:%M:%S")
}
str(in_time)

#Convert dates into proper format in out_time

for (j in 2:ncol(out_time))
{
  out_time[,j] <- as.POSIXct(out_time[,j], format = "%Y-%m-%d %H:%M:%S")
}

#Creating a dataset with number of hours worked per day

hrs_worked <- data.frame(out_time[,1])
colnames(hrs_worked)[1] <- "EmployeeID"

for (l in 2:ncol(out_time))
{
  hrs_worked[,l] <- round(difftime(out_time[,l], in_time[,l], units = "hours"),2)
}

colnames(hrs_worked) <- colnames(out_time)

#Aggregating number of hours worked and storing in a new column in "main" dataset

for (m in 1:nrow(main))
{
  main$hrs_worked[m] <- mean(as.numeric(hrs_worked[m,2:ncol(hrs_worked)]),na.rm = TRUE)
}

# Hence, "hrs_worked" is a derived metric that can be used in analysis

#Check for outliers in hrs_worked
quantile(main$hrs_worked, probs = seq(0,1,0.01))
# no outliers

#cleaning the data for NA values
#2.4 % observations are lost when this is run
main<- main[complete.cases(main),]

main_eda<- main

## Exploratory Data Analysis

#Introducing levels for Education
main_eda$Education <- factor(main_eda$Education)
levels(main_eda$Education) <- c("Below College", "College", "Bachelor", "Master", "Doctor")

main_eda$EnvironmentSatisfaction<- factor(main_eda$EnvironmentSatisfaction)
main_eda$JobInvolvement<- factor(main_eda$JobInvolvement)
main_eda$JobSatisfaction<- factor(main_eda$JobSatisfaction)

levels(main_eda$EnvironmentSatisfaction)<- c("Low", "Medium", "High", "Very High") 
levels(main_eda$JobInvolvement)<- c("Low", "Medium", "High", "Very High")
levels(main_eda$JobSatisfaction)<- c("Low", "Medium", "High", "Very High")

main_eda$WorkLifeBalance<- factor(main_eda$WorkLifeBalance)
levels(main_eda$WorkLifeBalance)<- c("Bad", "Good", "Better", "Best")

main_eda$PerformanceRating<- factor(main_eda$PerformanceRating)
levels(main_eda$PerformanceRating)<- c("Low", "Good", "Excellent", "Outstanding")

#the numerical variables we have are: 
#MonthlyIncome, hrs_worked, Age, DistanceFromHome, JobLevel, NumCompaniesWorked, PercentSalaryHike, TotalWorkingYears, TrainingTimesLastYear, YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager

summary(main_eda$MonthlyIncome)
summary(main_eda$hrs_worked)
summary(main_eda$Age)
summary(main_eda$DistanceFromHome)
#distance from home seems to be right skewed
summary(main_eda$NumCompaniesWorked)
summary(main_eda$PercentSalaryHike)
summary(main_eda$TotalWorkingYears)
summary(main_eda$TrainingTimesLastYear)
summary(main_eda$YearsAtCompany) 
summary(main_eda$YearsSinceLastPromotion)
summary(main_eda$YearsWithCurrManager)


#Bivariate Analysis
ggplot(main_eda, aes(x= hrs_worked, y= MonthlyIncome)) + geom_point()
ggplot(main_eda, aes(x= TotalWorkingYears, y= MonthlyIncome)) + geom_point()
ggplot(main_eda, aes(x= YearsAtCompany, y= MonthlyIncome)) + geom_point()

#Creating a correlation plot on the continous variables
numeric.var <- sapply(main_eda, is.numeric)
corr.matrix <- cor(main_eda[,numeric.var])
corrplot(corr.matrix, title = 'Correlation plot',
         method = "color", tl.cex = 0.7, type = "lower")

#YearsAtCompany, YearsSinceLastPromotion, YearsWithCurrManager are correlated
#total workingyears and age are also correlated

#How do continuous variables vary with attrition
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = scale(MonthlyIncome), color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = hrs_worked, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = Age, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = DistanceFromHome, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = NumCompaniesWorked, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = PercentSalaryHike, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = TotalWorkingYears, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = TrainingTimesLastYear, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = YearsAtCompany, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = YearsSinceLastPromotion, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 
ggplot(main_eda) +  geom_freqpoly(mapping = aes(x = YearsWithCurrManager, color = Attrition), binwidth = 1, size = 1.5) +  guides(fill=FALSE) 

#Categorical Variables
#Creating a function to output the percentage of attrition for various categorical variables.    
attr_var<- function(x, y){
  aaa<- enquo(x)
  bbb<- enquo(y)
  main_eda %>%
  group_by(!!aaa, !!bbb) %>%
  summarise(n = n()) %>%
  mutate(Percentage = n / sum(n) *100) %>%
  filter(!!bbb == "Yes")
  }


ggplot(attr_var(BusinessTravel, Attrition), aes(x= BusinessTravel, y= Percentage)) + geom_bar(stat = "identity", width = 0.5) + ggtitle("Percentage of Attrition")
ggplot(attr_var(Department, Attrition), aes(x= Department, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(Education, Attrition), aes(x= Education, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(EducationField, Attrition), aes(x= EducationField, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(Gender, Attrition), aes(x= Gender, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(JobLevel, Attrition), aes(x= JobLevel, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(JobRole, Attrition), aes(x= JobRole, y= Percentage)) + geom_bar(stat = "identity", width = 0.5) +coord_flip()
ggplot(attr_var(MaritalStatus, Attrition), aes(x= MaritalStatus, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(StockOptionLevel, Attrition), aes(x= StockOptionLevel, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(EnvironmentSatisfaction, Attrition), aes(x= EnvironmentSatisfaction, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(WorkLifeBalance, Attrition), aes(x= WorkLifeBalance, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(JobSatisfaction, Attrition), aes(x= JobSatisfaction, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(JobInvolvement, Attrition), aes(x= JobInvolvement, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)
ggplot(attr_var(PerformanceRating, Attrition), aes(x= PerformanceRating, y= Percentage)) + geom_bar(stat = "identity", width = 0.5)



#Important Attrition Predictors:
#Business Travel
#Education Field
#Department
#JobRole- research directors more likely
#Marital Status
#LowEnvironmentSatisfaction
#BadWorkLifeBalance
#JobSatisafaction
#Jobinvolvemnet- low and very high not mch diff in attrition
#good performance rating more likely to attrition?

#What other factors influence Environmentsatisfaction
#YearsSinceLastPromotion"
#"YearsWithCurrManager" #"PercentSalaryHike" , #Monthly Income, 
#hrs_worked, 
#Age, 
#DistanceFromHome, 

ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= YearsSinceLastPromotion)) + coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= YearsWithCurrManager))+ coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= MonthlyIncome)) + coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= hrs_worked))+ coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= Age)) + coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= DistanceFromHome))+ coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= NumCompaniesWorked))+ coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= PercentSalaryHike))+ coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= TotalWorkingYears))+ coord_flip() 
ggplot(main_eda, aes(x=EnvironmentSatisfaction)) + geom_boxplot(aes(y= TrainingTimesLastYear))+ coord_flip() 
#low satisfaction, = low hours worked

ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= YearsSinceLastPromotion)) + coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= YearsWithCurrManager))+ coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= MonthlyIncome)) + coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= hrs_worked))+ coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= Age)) + coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= DistanceFromHome))+ coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= NumCompaniesWorked))+ coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= PercentSalaryHike))+ coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= TotalWorkingYears))+ coord_flip() 
ggplot(main_eda, aes(x=WorkLifeBalance)) + geom_boxplot(aes(y= TrainingTimesLastYear))+ coord_flip() 

#distance from home could be one predictor of worklife balance

ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= YearsSinceLastPromotion)) + coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= YearsWithCurrManager))+ coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= MonthlyIncome)) + coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= hrs_worked))+ coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= Age)) + coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= DistanceFromHome))+ coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= NumCompaniesWorked))+ coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= PercentSalaryHike))+ coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= TotalWorkingYears))+ coord_flip() 
ggplot(main_eda, aes(x=JobSatisfaction)) + geom_boxplot(aes(y= TrainingTimesLastYear))+ coord_flip() 

#no marked difference across any of the categoriess

ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= YearsSinceLastPromotion))+ facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= YearsWithCurrManager))+ facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= MonthlyIncome)) + facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= hrs_worked))+ facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= Age)) + facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= DistanceFromHome))+ facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= NumCompaniesWorked))+ facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= PercentSalaryHike))+ facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= TotalWorkingYears))+ facet_grid(~main_eda$Attrition)
ggplot(main_eda, aes(x=JobInvolvement)) + geom_boxplot(aes(y= TrainingTimesLastYear))+ facet_grid(~main_eda$Attrition)

#years with current manager and hours worked show marked differences
#Monthly Income, 
#hrs_worked, 
#Age, 
#DistanceFromHome, 
#JobLevel, 
#"NumCompaniesWorked"     
#"PercentSalaryHike" 
#"TotalWorkingYears"       
#"TrainingTimesLastYear"  
#"YearsAtCompany"          
#"YearsSinceLastPromotion"
#"YearsWithCurrManager"


#, worklifebalance, job satisafction and involvement and performance?

# [4] "BusinessTravel"          "Department"             
# [7] "Education"               "EducationField"          "Gender"                 
# [10] "JobLevel"                "JobRole"                 "MaritalStatus"          
# [16] "StockOptionLevel"        
# [22] "EnvironmentSatisfaction" "JobSatisfaction"         "WorkLifeBalance"        
# [25] "JobInvolvement"          "PerformanceRating"  

#are males and females differ by marital status and attrition
ggplot(main_eda) + geom_bar(aes(x = MaritalStatus, fill = Attrition), position = "fill") + 
  facet_wrap(~Gender) 

######################################################################################################

## Data Preparation for Logistic Regression

#Standardization of values of numeric variables
main$MonthlyIncome <- round(scale(main$MonthlyIncome),2)
main$hrs_worked <- round(scale(main$hrs_worked),2)


#Convert the value of attrition variable into 0s and 1s
main$Attrition <- ifelse(main$Attrition=="Yes", 1,0)
# Checking attrition rate of employee
attrition <- sum(main$Attrition)/nrow(main)
attrition # 16.16% attrition rate. 

# creating a dataframe of categorical features
main_cat<- main[,c(4,5,8,9,11,12)]
str(main_cat)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(main_cat, 
                            function(x) data.frame(model.matrix(~x-1,data =main_cat))[,-1]))

#merging the dummy variables with main dataset
main <- cbind(main[,-c(4,5,8,9,11,12)], dummies)


######################################################################################################
# Final dataset
main_final <- main[,-1] #Removing identifier variable "EmployeeID"
View(main_final) 
str(main_final) #4300 obs. of  40 variables

#############

# splitting the data between train and test
set.seed(100)
indices = sample.split(main_final$Attrition, SplitRatio = 0.7)
train = main_final[indices,]
test = main_final[!(indices),]


######################################################################################################

# Logistic Regression: 

#Building the model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1)  
#Null deviance: 2661.4  on 3009  degrees of freedom
#Residual deviance: 2059.2  on 2970  degrees of freedom
#AIC: 2139.2

# Stepwise selection
model_2<- stepAIC(model_1, direction="both")
summary(model_2)

#final model by AIC 
#Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + 
#TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
#  YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
#  WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
#  BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
#  EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
#  EducationField.xTechnical.Degree + JobRole.xHuman.Resources + 
#  JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
#  JobRole.xSales.Executive + MaritalStatus.xMarried + MaritalStatus.xSingle
# final AIC=2122.74

#checking multicollinearity through VIF check
vif(model_2)

  
##MaritalStatus.xMarried is not significant at 95% confidence interval and has a VIF of 2.10

model_3<- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                EducationField.xTechnical.Degree + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, 
              data = train, family = "binomial")

              
summary(model_3) 
#Null deviance: 2661.4  on 3009  degrees of freedom
#Residual deviance: 2071.5  on 2984  degrees of freedom
#AIC: 2123.5

#checking for multicollinearity:
vif(model_3) 

#EducationField.xTechnical.Degree is insiginificant and has high VIF
#Removing that

model_4<-  glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                 BusinessTravel.xTravel_Rarely + EducationField.xLife.Sciences + 
                 EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, 
               data = train, family = "binomial")

summary(model_4) 

# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2089.7  on 2985  degrees of freedom
# AIC: 2139.7

vif(model_4) 

#EducationField.xLife.Sciences has high VIF and is insignificant

model_5<- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                BusinessTravel.xTravel_Rarely + 
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, 
              data = train, family = "binomial")
summary(model_5)
# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2089.7  on 2986  degrees of freedom
# AIC: 2137.7

vif(model_5)
#BusinessTravel.xTravel_Rarely has a vif of 3
# Removing it in the next model:
model_6<- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                EducationField.xMarketing + EducationField.xMedical + EducationField.xOther + 
                + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, 
              data = train, family = "binomial")
summary(model_6)

vif(model_6)

#EducationField.xMarketing has the highest p-value, hence removing it

model_7<- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                + EducationField.xMedical + EducationField.xOther + 
                + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, 
              data = train, family = "binomial")
summary(model_7)

# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2099.2  on 2988  degrees of freedom
# AIC: 2143.2
vif(model_7)

#EducationField.xMedical has the highest p-value, hence removing it.

model_8<- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + 
                TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                + EducationField.xOther + 
                + JobRole.xHuman.Resources + 
                JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                JobRole.xSales.Executive + MaritalStatus.xSingle, 
              data = train, family = "binomial")
summary(model_8)

# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2099.5  on 2989  degrees of freedom
# AIC: 2141.5

vif(model_8)

# EducationField.xOther has high p-value. Exlcuding it
#Excluding it

model_9<- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + StockOptionLevel + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, 
               data = train, family = "binomial")
summary(model_9)
 
# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2100.8  on 2990  degrees of freedom
# AIC: 2140.8

vif(model_9)

#Excluding StockOptionLevel as it has a high p-value

model_10<- glm(Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, 
               data = train, family = "binomial")
summary(model_10)

# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2103.2  on 2991  degrees of freedom
# AIC: 2141.2

vif(model_10)

#Excluding JobLevel as it has a high p-value

model_11<- glm(Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                 + JobRole.xHuman.Resources + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, 
               data = train, family = "binomial")
summary(model_11)

# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2105.8  on 2992  degrees of freedom
# AIC: 2141.8
vif(model_11)

#Removing variables with less than three stars significance one by one as per p-value

#Excluding JobRole.xHuman.Resources

model_12<- glm(Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                 JobRole.xManager + JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, 
               data = train, family = "binomial")
summary(model_12)

# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2109.0  on 2993  degrees of freedom
# AIC: 2143
vif(model_12)

#Excluding JobRole.xManager

model_13<- glm(Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 JobRole.xSales.Executive + MaritalStatus.xSingle, 
               data = train, family = "binomial")
summary(model_13)

vif(model_13)

#Excluding JobRole.xSales.Executive

model_14<- glm(Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + JobRole.xResearch.Director + 
                 MaritalStatus.xSingle, 
               data = train, family = "binomial")
summary(model_14)
 
# Null deviance: 2661.4  on 3009  degrees of freedom
# Residual deviance: 2120.2  on 2995  degrees of freedom
# AIC: 2150.2

vif(model_14)

#Excluding JobRole.xResearch.Director

model_15<- glm(Attrition ~ Age + NumCompaniesWorked + 
                 TotalWorkingYears + TrainingTimesLastYear + YearsSinceLastPromotion + 
                 YearsWithCurrManager + EnvironmentSatisfaction + JobSatisfaction + 
                 WorkLifeBalance + hrs_worked + BusinessTravel.xTravel_Frequently + 
                 JobRole.xManufacturing.Director + 
                 MaritalStatus.xSingle, 
               data = train, family = "binomial")
summary(model_15)

vif(model_15)

#Since all variables have a low VIF and significant p-value
#model_15 is our final model with 11 significant variables

final_model <- model_15

### Model Evaluation ###

#how does this model perform on the training data?
train$pred = predict(final_model, train, type = "response")

#prediction on test data
test_pred = predict(final_model, type = "response", 
                    newdata = test[,-2])

# Let's see the summary 

summary(test_pred)

# The probabilities range between 0.01% to 88%
# Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.0002425 0.0402212 0.1025034 0.1661968 0.2364135 0.8633642 

test$prob <- test_pred
View(test)

# Checking with the probability cutoff of 50%

test_pred_attrition <- factor(ifelse(test_pred >= 0.50, "Yes", "No"))
test_actual_attrition <- factor(ifelse(test$Attrition == 1,"Yes","No"))


table(test_actual_attrition,test_pred_attrition)


# test_pred_attrition
# test_actual_attrition   No  Yes
#                   No  1046   35
#                   Yes  151   58

test_conf <- caret::confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
# Here the accuracy = 85%, sensitivity = 27% and specificity = 97%
# Since we have to predict the occurance of employees leaving i.e attrition = "Yes", we need to increase our 
# Sensitivity

# Changing the threshold to 0.40 and checking for accuracy, sensitivity, specificity

test_pred_attrition <- factor(ifelse(test_pred >= 0.40, "Yes", "No"))
test_conf <- caret::confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
# Here the accuracy = 86%, sensitivity = 39% and specificity = 95%
# We observe an increase in the sensitivity

test_pred_attrition <- factor(ifelse(test_pred >= 0.30, "Yes", "No"))
test_conf <- caret::confusionMatrix(test_pred_attrition, test_actual_attrition, positive = "Yes")
test_conf
# Here the accuracy = 84%, sensitivity = 59% and specificity = 88%
# We observe an increase in the sensitivity

# Finding the optimal probability cutoff value

cutoff_fn <- function(cutoff) 
{
  predicted_attrition <- factor(ifelse(test_pred >= cutoff, "Yes", "No"))
  conf <- caret::confusionMatrix(predicted_attrition, test_actual_attrition, positive = "Yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

# initiallizing a matrix of 100 X 3.

# Summary of test probability

summary(test_pred)

s = seq(.01,.80,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = cutoff_fn(s[i])
} 


plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))
box()


cutoff <- s[which(abs(OUT[,1]-OUT[,2])<0.01)]
cutoff  

# Choosing a cutoff value of 0.177 for final model

test_cutoff_attrition <- factor(ifelse(test_pred >=0.1776, "Yes", "No"))
conf_final <- caret::confusionMatrix(test_cutoff_attrition, test_actual_attrition, positive = "Yes")

acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

acc # accuracy:  # 76%
sens # sensitivity: # 76%
spec # specificity: # 76%

View(test)

#####################################################################################################

### KS -statistic - Test Data #######################################################################

test_cutoff_attrition <- ifelse(test_cutoff_attrition =="Yes", 1, 0)
test_actual_attrition <- ifelse(test_actual_attrition =="Yes", 1, 0)

##### on testing  data ##############################################################################

pred_object_test<- prediction(test_cutoff_attrition,test_actual_attrition)

performance_measures_test<- performance(pred_object_test, "tpr", "fpr")

ks_table_test <- attr(performance_measures_test, "y.values")[[1]] - 
  (attr(performance_measures_test, "x.values")[[1]])

max(ks_table_test)

## The KS Statistic  = 51% 

#####################################################################################################
# Lift & Gain Chart 

# Plotting the Lift chart


lift <- function(labels , predicted_prob,groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalattrn = sum(., na.rm = TRUE))) %>%
    
    mutate(Cumattrn = cumsum(totalattrn),
           Gain=Cumattrn/sum(totalattrn)*100,
           Cumlift=Gain/(bucket*(100/groups))) 
  return(gaintable)
}

Attrition_decile = lift(test_actual_attrition, test_pred, groups = 10)

ggplot(Attrition_decile, aes(x = bucket, y = Gain)) + geom_line()

##################################################################################################################


