#loading required packages

library(dplyr)
library(ggplot2)
library(MASS)
library(car)

#data extraction

main <- read.csv("general_data.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
employee <- read.csv("employee_survey_data.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
manager <- read.csv("manager_survey_data.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
in_time <- read.csv("in_time.csv", stringsAsFactors = FALSE, na.strings = c("",NA))
out_time <- read.csv("out_time.csv", stringsAsFactors = FALSE, na.strings = c("",NA))


#Check if there are duplicates
sum(duplicated(main$EmployeeID))

#Check if EmployeeID is same in datasets to be merged
setdiff(main$EmployeeID, employee$EmployeeID) #Employee ID's are identical
setdiff(main$EmployeeID, manager$EmployeeID) #Employee ID's are identical

#Merge required datasets
main <- merge(main, employee, by = "EmployeeID")
main <- merge(main, manager, by = "EmployeeID")

str(main)

#Checking for NA values
sapply(main, function(x) length(which(is.na(x))))

#Convert the value of attrition variable into 0s and 1s

main$Attrition <- ifelse(main$Attrition=="Yes", 1,0)

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

summary(main)

####  Check for outliers ####

#Outliers in MonthlyIncome
quantile(main$MonthlyIncome, probs = seq(0,1,0.01))
# capping till 2% quantile
main$MonthlyIncome <- sapply(main$MonthlyIncome, function(x) ifelse(x < 18590.0 ,18590.0,x))


#### DERIVED METRIC ####

#change 1st column's column name in in_time and out_time to "EmployeeID"

colnames(in_time)[1] <- "EmployeeID"
colnames(out_time)[1] <- "EmployeeID"

#Convert dates into proper format in in_time

for (i in 2:ncol(in_time))
{
  in_time[,i] <- as.POSIXct(in_time[,i], format = "%Y-%m-%d %H:%M")
}


#Convert dates into proper format in out_time

for (j in 2:ncol(out_time))
{
  out_time[,j] <- as.POSIXct(out_time[,j], format = "%Y-%m-%d %H:%M")
}

#Creating a dataset with number of hours worked per day

hrs_worked <- data.frame(out_time[,1])
colnames(hrs_worked)[1] <- "EmployeeID"

for (l in 2:ncol(out_time))
{
  hrs_worked[,l] <- round(difftime(out_time[,l], in_time[,l], units = "hours"),2)
}

colnames(hrs_worked) <- colnames(out_time)

#Aggregating number of hours worked and store in a new column in "main" dataset

for (m in 1:nrow(main))
{
  main$hrs_worked[m] <- sum(as.numeric(hrs_worked[m,2:ncol(hrs_worked)]),na.rm = TRUE)
}

# Hence, "hrs_worked" is a derived metric that can be used in analysis

#Check for outliers in hrs_worked
quantile(main$hrs_worked, probs = seq(0,1,0.01))
# no outliers


# Barcharts for categorical features with stacked firm information

bar_theme1<- theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
                   legend.position="none")

library(cowplot)

plot_grid(ggplot(main, aes(x=BusinessTravel,fill=Attrition))+ geom_bar() + bar_theme1, 
          ggplot(main, aes(x=JobRole,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(main, aes(x=Department,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(main, aes(x=MaritalStatus,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(main, aes(x=EducationField,fill=Attrition))+ geom_bar()+bar_theme1,
          ggplot(main, aes(x=Gender,fill=Attrition))+ geom_bar()+bar_theme1,
          align = "h")   

# Histogram and Boxplots for numeric variables 
box_theme<- theme(axis.line=element_blank(),axis.title=element_blank(), 
                  axis.ticks=element_blank(), axis.text=element_blank())

box_theme_y<- theme(axis.line.y=element_blank(),axis.title.y=element_blank(), 
                    axis.ticks.y=element_blank(), axis.text.y=element_blank(),
                    legend.position="none")

plot_grid(ggplot(main, aes(MonthlyIncome))+ geom_histogram(binwidth = 10),
          ggplot(main, aes(x="",y=MonthlyIncome))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(main, aes(hrs_worked))+ geom_histogram(binwidth = 20),
          ggplot(main, aes(x="",y=hrs_worked))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1)

plot_grid(ggplot(telecom, aes(TotalCharges))+ geom_histogram(),
          ggplot(telecom, aes(x="",y=TotalCharges))+ geom_boxplot(width=0.1)+coord_flip()+box_theme, 
          align = "v",ncol = 1) 


plot_grid(ggplot(main, aes(x=factor(Attrition),y=MonthlyIncome, fill=Attrition))+ geom_boxplot(width=0.2)+ 
            coord_flip() +theme(legend.position="none"),
          ggplot(main, aes(x=factor(Attrition),y=hrs_worked, fill=Attrition))+ geom_boxplot(width=0.2)+
            coord_flip() + box_theme_y,
          align = "v",nrow = 1)


#checking for correlation between numeric variables
library(GGally)
ggpairs(main[, c("MonthlyIncome", "hrs_worked")]) #they are not correlated


#Standardization of values of numeric variables
main$MonthlyIncome <- round(scale(main$MonthlyIncome),2)
main$hrs_worked <- round(scale(main$hrs_worked),2)

# Checking attrition rate of employee

attrition <- sum(main$Attrition)/nrow(main)
attrition # 16.12% attrition rate. 

# creating a dataframe of categorical features
main_cat<- main[,c(4,5,8,10,12,13)]
str(main_cat)

# creating dummy variables for factor attributes
dummies<- data.frame(sapply(main_cat, 
                            function(x) data.frame(model.matrix(~x-1,data =main_cat))[,-1]))

#merging the dummy variables with main dataset
main <- cbind(main[,-c(4,5,8,10,12,13)], dummies)

# Final dataset
main_final <- main[,-1] #Removing identifier variable "EmployeeID"
View(main_final) #4410 obs. of  41 variables

#############

# splitting the data between train and test
set.seed(100)

library(caTools)
indices = sample.split(main_final$Attrition, SplitRatio = 0.7)

train = main_final[indices,]

test = main_final[!(indices),]


###########

# Logistic Regression: 

#Initial model
model_1 = glm(Attrition ~ ., data = train, family = "binomial")
summary(model_1) #AIC 2148.1...40 coeff...nullDev 2662.8...resDev 2068.1

# Stepwise selection
library("MASS")
model_2<- stepAIC(model_1, direction="both")

summary(model_2)

# Removing multicollinearity through VIF check
library(car)
vif(model_2)

#Plot the correlations
library(corrplot)
cor(main_final[,c(22,23,24,25)])
corrplot(cor(main_final[,c(22,23,24,25)]))

#Department Sales and Department R&D are highly negatively correlated 
#followed by Business Travel Frequently and Business Travel Rarely


#Excluding Department Sales as it has higher p-value
model_3<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                PercentSalaryHike + StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                hrs_worked + BusinessTravel.xTravel_Frequently + BusinessTravel.xTravel_Rarely + 
                Department.xResearch...Development + EducationField.xOther + JobRole.xManufacturing.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + EducationField.xTechnical.Degree, 
              family = "binomial", data = train)
summary(model_3) 

vif(model_3) 

#We found that the next highest correlation was between Business Travel Frequently and Business Travel Rarely

#Excluding Business Travel Rarely as it has higher p-value

model_4<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                PercentSalaryHike + StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                hrs_worked + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + EducationField.xOther + JobRole.xManufacturing.Director + 
                MaritalStatus.xMarried + MaritalStatus.xSingle + EducationField.xTechnical.Degree, 
              family = "binomial", data = train)
summary(model_4) 

vif(model_4) 


# Marital Statuses have slightly higher VIF
#Let's check for correlation

cor(main_final$MaritalStatus.xSingle,main_final$MaritalStatus.xMarried)
#They are moderately positively correlated

# Excluding Marital Staus Married as it has lower p-value 
model_5<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                PercentSalaryHike + StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                hrs_worked + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + EducationField.xOther + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EducationField.xTechnical.Degree, 
              family = "binomial", data = train)
summary(model_5)

vif(model_5)

#Total Working Years has VIF greater than 2
#But it has significant p-value
# So we keep it

#Let's look that the p-values

#PercentSalaryHike has very high p-value. Let's remove it.

model_6<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                hrs_worked + BusinessTravel.xTravel_Frequently +
                Department.xResearch...Development + EducationField.xOther + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EducationField.xTechnical.Degree, 
              family = "binomial", data = train)
summary(model_6)

vif(model_6)

#Development Research...Development has the next highest p-value.
#Excluding it

model_7<- glm(formula = Attrition ~ Age + JobLevel + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                hrs_worked + BusinessTravel.xTravel_Frequently +
                EducationField.xOther + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EducationField.xTechnical.Degree, 
              family = "binomial", data = train)
summary(model_7)

vif(model_7)

#Job level also has a high p-value
#Excluding it

model_8<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                hrs_worked + BusinessTravel.xTravel_Frequently +
                EducationField.xOther + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle + EducationField.xTechnical.Degree, 
              family = "binomial", data = train)
summary(model_8)

vif(model_8)

# EducationField.xTechnical.Degree has a high p-value
#Excluding it

model_9<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                hrs_worked + BusinessTravel.xTravel_Frequently +
                EducationField.xOther + JobRole.xManufacturing.Director + 
                MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_9)

vif(model_9)

#Excluding Education Field Other as it has a high p-value

model_10<- glm(formula = Attrition ~ Age + NumCompaniesWorked + 
                StockOptionLevel + TotalWorkingYears + 
                TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                hrs_worked + BusinessTravel.xTravel_Frequently +
                JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
              family = "binomial", data = train)
summary(model_10)

vif(model_10)

#Excluding StockOptionLevel as it has a high p-value

model_11<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + YearsWithCurrManager + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 hrs_worked + BusinessTravel.xTravel_Frequently +
                 JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_11)

vif(model_11)

#All variable are significant, but VIF of TotalWorkingYears is slightly high
#Cheking for correlation

View(cor(main_final[,c(1,8,11,12,14,15,16,17,18,21,22,35,41)]))
#TotalWorkingYears is not correlated with any other variable

#But we find that YearSinceLastPromotion and YearsWithCurrManager are moderately correlated.

# Let's remove YearsWithCurrManager in the next model as it has high p-value among the two

model_12<- glm(formula = Attrition ~ Age + NumCompaniesWorked + TotalWorkingYears + 
                 TrainingTimesLastYear + YearsSinceLastPromotion + 
                 EnvironmentSatisfaction + JobSatisfaction + WorkLifeBalance + 
                 hrs_worked + BusinessTravel.xTravel_Frequently +
                 JobRole.xManufacturing.Director + MaritalStatus.xSingle, 
               family = "binomial", data = train)
summary(model_12)

vif(model_12)

#Since all variables have a low VIF and significant p-value
#model_12 is our final model
