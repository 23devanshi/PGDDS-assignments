#loading required libraries
library(ggplot2)
library(lubridate)
library(stringr)
#reading the data
loan<- read.csv("Course 2/Gramener Case Study/loan.csv")

#loan & member id is the unique id for this dataset
length(unique(loan$id))
length(unique(loan$member_id))

#------------------------------------------------------------------------
#--------------------Data Cleaning--------------------------------

#a lot of columns contain only NA values
#removing these variables before proceeding with further analysis
loan<- loan[, colSums(is.na(loan)) != nrow(loan)]

#Considering only the Charged off and Fully paid Statuses of loan
#i.e. removing rows where loan status is current
loan<- loan[loan$loan_status != "Current",]

##changing data type of columns
#4 columns that are date variables are stored as  factor
datevar<- c("issue_d","last_credit_pull_d", "next_pymnt_d", "last_pymnt_d", "earliest_cr_line")
loan[,datevar]<- lapply(loan[,datevar], function(x) parse_date_time(x, orders = "b y"))

#interest rate & revolving_util are stored as factor- need to convert it to integer
loan$int_rate<- as.numeric(lapply(loan$int_rate, function(x) str_split(x, "%")[[1]][1]))
loan$revol_util<- as.numeric(lapply(loan$revol_util, function(x) str_split(x, "%")[[1]][1]))

#changing purpose to a more coherent string- useful for plotting later
loan$purpose<- str_replace(loan$purpose, "_", " ")
  
#employment length is currently a factor with 11 levels- it could be condensed further
summary(loan$emp_length)
#there are about a 1075 rows with NA values
#regrouping it to a continuous variable 
loan$emp_length <- as.character(loan$emp_length)
loan$year_empl<- lapply(loan$emp_length, function(x) str_split(x, " year")[[1]][1])
loan$year_empl[loan$year_empl == "< 1"] <- 0
loan$year_empl[loan$year_empl == "10+"] <- 10
#those with years of employment equivalent to NA are being reassigned to 0 years of experience
loan$year_empl[is.na(loan$year_empl) ] <- 0
loan$year_empl <- as.integer(loan$year_empl)
summary(loan$year_empl)

#removing outliers in annual income
q1 = quantile(loan$annual_inc,probs = 0.25)
q3 = quantile(loan$annual_inc, probs = 0.75)
iqr1 = q3-q1
iqr2 = IQR(loan$annual_inc)

iqr1 == iqr2
upper = q3+1.5*iqr1
lower=  q1-1.5*iqr1
#only values containing in the range (lower, upper) will be used for analysis
loan<- loan[loan$annual_inc < upper & loan$annual_inc >lower,]

#length of credit history a/c for 15% of FICO score
#this can be seen as earliest credit line- issue date
loan$length_crh <- difftime(loan$issue_d, loan$earliest_cr_line, units = "weeks")


##DEFAULT
#defining default as the number of loans with status charged off
loan$default<- 0
loan$default[loan$loan_status == "Charged Off"] <- 1
#14% of loans given out ended in default
summary(loan$default)

#------------------------------------------------------------------------
#--------------------Univariate  Analysis--------------------------------

## Loan Amount
summary(loan$funded_amnt)
#histogram of funded amount
ggplot(loan, aes(x= funded_amnt))+
  geom_histogram() 

## Interest rate  
summary(loan$int_rate)
#histogram of interest rate
ggplot(loan, aes(x= int_rate))+
  geom_histogram(fill= "dodgerblue2", color= "black", binwidth = 1)+
  labs(title= "Distribution of Interest Rate", x= "Interest Rate", y = "Count")

#does interest rate vary by purpose?
ggplot(loan, aes(y= int_rate, x= purpose))+
  geom_boxplot() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  labs(title= "Variation of Interest Rate with Purpose", x= "Purpose", y = "Interest Rate")+
  theme(legend.position = "bottom")+
  theme(axis.text.x=element_text(angle=70, hjust=1))

#variation of interest rate by purpose
purpose<- aggregate(int_rate~ purpose, data= loan, FUN=function(x) {round(mean(x),2)})
ggplot(purpose, aes(x= reorder(purpose, -int_rate), y= int_rate)) +
  geom_bar(stat= "identity", fill= "dodgerblue2")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  labs(title= "Variation of Interest Rate with Purpose", x= "Purpose", y = "Interest Rate")+
  geom_text(data=purpose,aes(x=purpose,y=int_rate,label=int_rate), size=3, vjust=-0.5)+
  theme(axis.text.x=element_text(angle=70, hjust=1))


##funded amount against purpose
ggplot(loan, aes(y= funded_amnt, x= purpose))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=70, hjust=1))+
  labs(title= "Variation of Funded Amount with Purpose", x= "Purpose", y = "Funded Amount")

#what is the term of the loans?
#more loans have a 3 year term
summary(loan$term)

##grade of loans
#grade of loans- #quality score of loans based on credit history, collateral quality and likelihood of repayment
ggplot(loan, aes(x= grade))+
  geom_bar(fill= "dodgerblue2", width= 0.3)+
  labs(title= "Distribution of Loans against Grade", x= "Grade", y = "Count")+
  geom_text(stat = "count", aes(label= ..count..), vjust= -0.5, size= 3.5)

##outstanding principal
summary(loan$out_prncp)
ggplot(loan, aes(x= out_prncp))+
  geom_histogram()
sum(loan$out_prncp ==0)/nrow(loan)
#97% of the loans are those  with no outstanding principal

## years of Employment
summary(loan$year_empl)
#many applicants have employment length of 10years or more
ggplot(loan, aes(x= as.factor(year_empl)))+
  geom_bar(fill= "dodgerblue2",width= 0.3)+
  labs(title= "Distribution of Loans against Years of Employment", x= "Years of Employment", y = "Count")+
  geom_text(stat = "count", aes(label= ..count..), vjust= -0.5, size= 3.5)

##home ownership
summary(loan$home_ownership)

##income
summary(loan$annual_inc)

#there are cases where this is negative- need to probe more
summary(loan$length_crh)

#debt to income ratio of borrowers
summary(loan$dti)
#dpes dti vary by interest rate?
ggplot(loan, aes(x= dti, y= int_rate))+
  geom_point(mapping= aes(color= default))

#is DTI a predictor of default?
ggplot(loan, aes(y= loan_amnt, x= as.factor(default)))+
  geom_boxplot(width= 0.3)
  

#number of credit inquiries
summary(loan$inq_last_6mths)

#payment history
#collections in the last 12 months
summary(loan$collections_12_mths_ex_med)
#this is 0 for all applicants

#accounts that are now delinquent
summary(loan$acc_now_delinq)
#this is also 0 for all applicants

#tax liens
summary(loan$tax_liens)

#chargeoff within 12 months
summary(loan$chargeoff_within_12_mths)

#delinquent accounts in the last two years
summary(loan$delinq_amnt)
summary(loan$delinq_2yrs)

##public record bankruptcies
summary(loan$pub_rec_bankruptcies)
ggplot(loan, aes(x= pub_rec_bankruptcies))+
  geom_bar() 
  
#what is the distribution of number of accounts?
summary(loan$open_acc)
ggplot(loan, aes(x= open_acc))+
  geom_bar() 

#total open lines of credit
summary(loan$total_acc)
ggplot(loan, aes(x= total_acc))+
  geom_bar() 

ggplot(loan, aes(y= (loan_amnt/annual_inc), x= as.factor(default)))+
  geom_boxplot(width= 0.5)

ggplot(loan, aes(y= (loan_amnt/annual_inc), x= dti))+
  geom_point()+
  geom_smooth()

loan$issue_year= format(loan$issue_d, "%Y")
year<- aggregate(default~issue_year, data= loan, FUN=function(x) {round(sum(x)/length(x),2)})
ggplot(year, aes(x=issue_year, y= default)) + 
  geom_bar(stat = "identity", width = 0.4, fill= "dodgerblue2")+
  labs(title= "Default Against the Years", x= "Year", y = "Defaults as a Proportion of Total Loans")+
  geom_text(data=year,aes(x=issue_year,y=default,label=default), size=4, vjust=-0.5)


#------------------------------------------------------------------------
#--------------------Bivariate  Analysis--------------------------------

## interest rate
ggplot(loan, aes(y= int_rate, x= as.factor(default)))+
  geom_boxplot(width= 0.2) +
  labs(title= "Variation of Interest Rate by Default", x= "Default", y = "Interest Rate")  


## funded amount 
ggplot(loan, aes(y= funded_amnt, x= as.factor(default))) +
  geom_boxplot(width= 0.2) +
  labs(title= "Variation of Loan Amount by Default", x= "Default", y = "Amount")  

##funded amount & default against purpose?
ggplot(loan, aes(y= funded_amnt, x= purpose, color= as.factor(default)))+
  geom_boxplot()

##term
ggplot(loan, aes(x= term, fill= as.factor(default)))+
  geom_bar(position = "dodge")+
  labs(title= "Term of Loans", x= "Term of Loans", y = "Count") 
#since there are more number of loans with a  3 year term, 
#we can look at the proportion of loans in each term that are defaulters
term<- aggregate(default~term, data= loan, FUN=function(x) {round(sum(x)/length(x),2)})
ggplot(term, aes(x=term, y= default)) + 
  geom_bar(stat = "identity", width = 0.3, fill= "dodgerblue2") +
  labs(title= "Interaction between Term of Loans & Default", x= "Term of Loans", y = "Defaults as a Proportion of Total Loans")+
  geom_text(data=term,aes(x=term,y=default,label=default), size=4, vjust=-0.5)
#there are more number of loans who defaulted on a three year term

##purpose of loan
purpose1<- aggregate(default~ purpose, data= loan, FUN=function(x) {round(sum(x)/length(x), 2)})
ggplot(purpose1, aes(x= reorder(purpose, -default), y= default)) +
  geom_bar(stat= "identity", fill= "dodgerblue2")+
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))+
  labs(title= "Proportion of Defaults Against Purpose", x= "Purpose", y = "Proportion of Defaults")+
  geom_text(data=purpose1,aes(x=purpose,y=default,label=default), size=3, vjust=-0.5)+
  theme(axis.text.x=element_text(angle=70, hjust=1))

#interaction between interest rate, purpose of loan & default
ggplot(loan, aes(y= int_rate, x= purpose, color= as.factor(default)))+
  geom_boxplot() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 7))+
  labs(title= "Interaction between Interest Rate, Purpose of Loan & Default", x= "Purpose", y = "Interest Rate")+
  theme(legend.position = "bottom")+
  theme(axis.text.x=element_text(angle=70, hjust=1))

## loan grade
grade<- aggregate(default~grade, data= loan, FUN=function(x) {round(sum(x)/length(x),2)})
ggplot(grade, aes(x=grade, y= default)) + geom_bar(stat = "identity", width = 0.4, fill= "dodgerblue2")+
  labs(title= "Interaction between Term of Loans & Default", x= "Grade", y = "Defaults as a Proportion of Total Loans")+
  geom_text(data=grade,aes(x=grade,y=default,label=default), size=3, vjust=-0.5)

##years of employment
ggplot(loan, aes(x= as.factor(default), y= year_empl))+
  geom_boxplot(width= 0.5)+
  labs(title= "Interaction between Years of Employment & Default", x= "Default", y = "Years of Employment")
##plotting it as a factor variable
ggplot(loan, aes(x= as.factor(year_empl), fill= as.factor(default)))+
  geom_bar(width= 0.5, position= "fill") 

yrsemp<- aggregate(default~as.factor(year_empl), data= loan, FUN=function(x) {round(sum(x)/length(x),2)})
names(yrsemp)[names(yrsemp)=="as.factor(year_empl)"] = "year_empl"
ggplot(yrsemp, aes(x=year_empl, y= default)) + 
  geom_bar(stat = "identity", width = 0.4, fill= "dodgerblue2")+
  labs(title= "Interaction between Years of Employment & Default", x= "Years of Employment", y = "Defaults as a Proportion of Total Loans")+
  geom_text(data=yrsemp,aes(x=year_empl,y=default,label=default), size=3, vjust=-0.5)

##home ownership
ggplot(loan, aes(x= home_ownership))+
  geom_bar()+
  facet_grid(.~default)
#plotting this as a bargraph against defaults
home_owner<- aggregate(default~home_ownership, data= loan, FUN=function(x) {round(sum(x)/length(x),2)})

ggplot(home_owner, aes(x=home_ownership, y= default)) + 
  geom_bar(stat = "identity", width = 0.4, fill= "dodgerblue2")+
  labs(title= "Interaction between Home Ownership & Default", x= "Home Ownership", y = "Defaults as a Proportion of Total Loans")+
  geom_text(data=home_owner,aes(x=home_ownership,y=default,label=default), size=3, vjust=-0.5)

loan$year_empl_range<- ifelse(loan$year_empl <=5, "0-5 Yrs", 
                              ifelse(loan$year_empl >5 & loan$year_empl <10, "5-10 Yrs", "10+ Yrs"))

#does the relationship change when years of employment are taken into account?
home_owner1<-aggregate(default~home_ownership + year_empl_range, data= loan, FUN=function(x) {round(sum(x)/length(x),2)})
home_owner1$yearempl_1<- factor(home_owner1$year_empl_range, levels = c("0-5 Yrs", "5-10 Yrs", "10+ Yrs"))
ggplot(home_owner1, aes(x=home_ownership, y= default)) + 
  geom_bar(stat = "identity", width = 0.4, fill= "dodgerblue2")+
  labs(title= "Interaction between Home Ownership, Years of Employment & Default", x= "Home Ownership", y = "Defaults as a Proportion of Total Loans")+
  geom_text(data=home_owner1,aes(x=home_ownership,y=default,label=default), size=3, vjust=-0.5)+
  facet_grid(.~yearempl_1)+
  theme(axis.text.x=element_text(angle=70, hjust=1))

##annual income
ggplot(loan, aes(x= as.factor(default), y= annual_inc))+
  geom_boxplot(width= 0.2)+
  labs(title= "Variation of Annual Income by Default", x= "Default", y = "Annual Income")

## length of credit history
ggplot(loan, aes(x= as.factor(default), y= length_crh))+
  geom_boxplot(width= 0.2)

## DTI
ggplot(loan, aes(y= dti, x= as.factor(default)))+
  geom_boxplot(width= 0.2)+
  labs(title= "Debt to Income Ratio versus Default", x= "Default", y = "Debt to Income Ratio")

##Revolving Utilisation
ggplot(loan, aes(x= as.factor(default), y= revol_util))+
  geom_boxplot(width= 0.2)+
  labs(title= "Revolving line utilization rate versus Default", x= "Default", y = "Revolving Line Utilisation Rate")

##credit inquiries
ggplot(loan, aes(y= inq_last_6mths, x= as.factor(default)))+
  geom_boxplot(width= 0.2)

##public record bankruptcies
ggplot(loan, aes(y= pub_rec_bankruptcies, x= as.factor(default)))+
  geom_boxplot() 

## Open credit Lines
ggplot(loan, aes(y= open_acc, x= as.factor(default)))+
  geom_boxplot(width= 0.2)+
  labs(title= "Number of Open Credit Lines versus Default", x= "Default", y = "Number of Open Credit Lines") 

## Total Credit Lines
ggplot(loan, aes(y= total_acc, x= as.factor(default)))+
  geom_boxplot(width= 0.2)+
  labs(title= "Number of Total Credit Lines versus Default", x= "Default", y = "Number of Total Credit Lines")  



