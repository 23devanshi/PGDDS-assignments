# Preliminaries
setwd("D:/Downloads")

#install.packages("dummies")
# Loding Packages
pkgs <- c("ggplot2", "MASS", "car", "caret", "caTools", "dummies", "dplyr")

data.frame(IS_LOADED = sapply(pkgs, function(p){library(p, character.only = T, 
                                                        logical.return = T)}))

##------------Loading the Data---------------------##

# Loading bank marketing data in the working directory. 
bank_data<- read.csv("bank_marketing.csv")

# Checking structure of dataset 
str(bank_data)

# Summary of dataset
summary(bank_data)

## ----------------------------------------------------------------------------------------------------------------
## ---------------------------------------------- Assigment Task 1 ---------------------------------------------- ##
# Data Preparation

# Data Cleaning

# Checking missing values
sum(is.na(bank_data))

# Outlier treatment
# Plotting Age histogram
ggplot(bank_data,aes(age))+geom_histogram()

# Let's check the outlier in the variables 
quantile(bank_data$age,seq(0,1,0.01))

# Box plot 
boxplot(bank_data$age)

# Capping the upper values of age with 71.
bank_data[(which(bank_data$age>71)),]$age <- 71

# Binning the age variable and store it into "binning.age".
bank_data$binning.age <- as.factor(cut(bank_data$age, breaks = c(16, 20, 30, 40, 50, 60, 70, 80)))

# Change the response value to numbers i.e"yes-no" to "1-0"
bank_data$response <- ifelse(bank_data$response == "yes", 1, 0)

# Check the numeric value of response rate in each bucket
agg_age <- merge(aggregate(response ~ binning.age, bank_data, mean),aggregate(response~binning.age, bank_data, sum),by = "binning.age") 

# Adding No.of_prospect
count <- data.frame(table(bank_data$binning.age))
count <- count[,-1]
agg_age <- cbind(agg_age,count)

# Removing Binning Variable
bank_data$binning.age <- NULL

# changing column name of each variables in agg_age dataframe
colnames(agg_age) <- c("age", "response_rate", "count_prospects","No.of_prospect")

# Round Off the values
agg_age$response_rate <- format(round(agg_age$response_rate, 2))
agg_age

# Let's see the response rate of each age bucket in the plot
ggplot(agg_age, aes(age, No.of_prospect,label = response_rate)) + 
  geom_bar(stat = 'identity') + theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
  geom_text(size = 3, vjust = -0.5)

# Let's check the dataset of age less than 20 years. 
Bank_data_age20 <- subset(bank_data,age <20)

View(Bank_data_age20)
summary(Bank_data_age20)

#-----Next Variable is "job"

# Checking the levels of the job
levels(bank_data$job)

# Plotting bar graph for job variable.
# Writing a function "plot_response" to do the same task for each variable

plot_response <- function(cat_var, var_name){
  a <- aggregate(response~cat_var, bank_data, mean)
  count <- data.frame(table(cat_var))
  count <- count[,-1]
  agg_response <- cbind(a, count)
  
  colnames(agg_response) <- c(var_name, "response_rate","No.of_Prospect")
  agg_response[, 2] <- format(round(agg_response[, 2], 2))
  
  ggplot(agg_response, aes(agg_response[, 1], count, label = response_rate)) + 
    geom_bar(stat = 'identity') + 
    theme(axis.text.x = element_text(angle = 60, hjust = 1)) + 
    geom_text(size = 3, vjust = -0.5) + xlab(var_name)
  
}

plot_response(bank_data$job, "job")

# Checking Marital status
summary(bank_data$marital)

# Let's replace Unknown level to married
levels(bank_data$marital)[4] <- "married"

# Plotting marital status
plot_response(bank_data$marital,"marital")

# Let's see the education variables
plot_response(bank_data$education,"Education")

# Reducing the levels of education variable
levels(bank_data$education)[c(1:3,5)] <- "Primary_Education"
levels(bank_data$education)[2] <- "Secondary_Education"
levels(bank_data$education)[4]<- "Tertiary_Education"

# Let's again check the education plot
plot_response(bank_data$education,"Education_levels")

# Let's see the default variable
table(bank_data$default)

plot_response(bank_data$default, "Default")
bank_data <- bank_data[,-5]

# Let's understand the housing variables 
summary(bank_data$housing)
plot_response(bank_data$housing, "Housing")

#-- Let's see the next variable which is "loan"
summary(bank_data$loan)
plot_response(bank_data$loan, "Loan Status")

#  Next variable is Contact, Let's see the response rate of each mode 
summary(bank_data$contact)
plot_response(bank_data$contact,"Contact_mode")

# Next variable is "Month" i.e contact month. 
plot_response(bank_data$month,"Contact_month")

# Let's do the same of "day_of_week" variable
plot_response(bank_data$day_of_week,"day_of_week")

# Now, Let's see the "duration" variable: Which is Quantitative variable
# Let's check the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()
# Let's see the summary of this variable once 
summary(bank_data$duration)
# Average duration 
bank_data$response_1 <- as.factor(bank_data$response)
Avg_duration <- aggregate(duration~response_1,bank_data,mean)
#removing the response_1 variable
bank_data <- subset(bank_data, select = -response_1)
## Definitely the outlier is present in the dataset
# So let's check the percentile distribution of duration 
quantile(bank_data$duration,seq(0,1,0.01))
# So, capping the duration seconds at 99% which is 1271.3sec 
bank_data[(which(bank_data$duration>1271.13)),]$duration <- 1271.13
# Now, again plot the histogram 
ggplot(bank_data,aes(duration))+geom_histogram()

# the next variable is "campaign" variable
#(number of contacts performed during this campaign and for this client 
# numeric, includes last contact)

# So let's check the summay of this variable 
summary(bank_data$campaign)
# Let's see the percentile distribution of this variable
boxplot(bank_data$campaign)
quantile(bank_data$campaign,seq(0,1,0.01))
# Capping this at 99% which the value is 14
bank_data[which(bank_data$campaign>14),]$campaign <- 14
# Visualizing it with plot
ggplot(bank_data,aes(campaign))+geom_histogram()

#-- Next variable is "pdays"
# Let's first convert this variable to factor type
bank_data$pdays<- as.factor(bank_data$pdays)
# Checking summary
summary(bank_data$pdays)
levels(bank_data$pdays)
# Reducing the levels of this variable to 3.
levels(bank_data$pdays)[1:10] <- "Contacted_in_first_10days"
levels(bank_data$pdays)[2:17] <-"Contacted_after_10days"
levels(bank_data$pdays)[3] <- "First_time_contacted"
# Also,lets see the respose rate of each levels. 
plot_response(bank_data$pday,"Pday")
# Number of prospects under each category
table(bank_data$pdays)

# Next variable is "previous" i.e number of contacts performed before 
# this campaign and for this client (numeric)
summary(bank_data$previous)
# Max=7, best is to convert this variable to factor
bank_data$previous <- as.factor(bank_data$previous)
levels(bank_data$previous)[1]<-"Never contacted"
levels(bank_data$previous)[2:4] <- "Less_than_3_times"
levels(bank_data$previous)[3:6] <- "More than_3_times"
summary(bank_data$previous)
plot_response(bank_data$previous,"Previous_contacts")

# Now, the next variable is "Poutcome" i.e  outcome of the previous marketing campaign 
# (categorical: 'failure','nonexistent','success')
summary(bank_data$poutcome)
plot_response(bank_data$poutcome,"Outcome_of_Previous_contacts")

#---------------------------------------------------------------------------
# Creating a Unique Identifier for each prospect
bank_data$id <- seq.int(nrow(bank_data))

## ----------------------------------------------------------------------------------------------------------------
## ---------------------------------------------- Assigment Task 2 ---------------------------------------------- ##
# Build a logistic regression model without using the variable 'duration'
# Perform variable selection using the usual methods
# Sort the data points in decreasing order of probability of response
# Find the optimal probability cut-off and report the relevant evaluation metrics

# Creating dummy variables
# Converting target variable to integer so that dummy variables are not created automatically.
View(bank_data)

bank_data$response <- as.integer(bank_data$response)
summary(bank_data$response)

bank_data_bkp <- bank_data  # backup dataset, to use if needed.
bank_data <- dummy.data.frame(bank_data)

# Converting the response variableback to factor.
bank_data$response <- as.factor(ifelse(bank_data$response == 1, "yes", "no"))

# splitting into train and test data

set.seed(100)
split_indices <- sample.split(bank_data$response, SplitRatio = 0.70)
train <- bank_data[split_indices, ]
test <- bank_data[!split_indices, ]

## Removing duration & prospectId from modelling as required.
## Also removing ProspectId from the model through it appears as significant if you include in the model as it does not make sense to include the Unique ID variable.

logistic_1 <- glm(response ~ . - duration - id, family = "binomial", data = train)
## Calling Stepwise Regression to remove variables for multi-cillinearity and insighnificance

# logistic_2 <- stepAIC(logistic_1, direction = "both")

# StepAIC has removed the variables and resulted in the new model.

logistic_2 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + day_of_weektue + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    previousLess_than_3_times + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + euribor3m + nr.employed + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, family = "binomial", data = train)

summary(logistic_2)
vif(logistic_2)

# Removing euribor3m from the model
logistic_3 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + day_of_weektue + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    previousLess_than_3_times + poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_3)
vif(logistic_3)

# Removing previousLess_than_3_times

logistic_4 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + day_of_weektue + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    educationTertiary_Education + `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_4)
vif(logistic_4)

## All high VIF variables are significant
# Removing educationTertiary_Education as highest p-value

logistic_5 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weekthu + day_of_weektue + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_5)
vif(logistic_5)

## All high VIF variables are significant
# Removing day_of_weekthu as highest p-value


logistic_6 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + day_of_weektue + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_6)
vif(logistic_6)

## All high VIF variables are significant
# Removing day_of_weekthu as highest p-value

logistic_7 <- glm(formula = response ~ age + jobretired + loanno + contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_7)
vif(logistic_7)

## All high VIF variables are significant
# Removing loanno as highest p-value


logistic_8 <- glm(formula = response ~ age + jobretired + contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_8)
vif(logistic_8)

# Removing jobretired

logistic_9 <- glm(formula = response ~ age + contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_9)
vif(logistic_9)

#Removing age

logistic_10 <- glm(formula = response ~ contactcellular + monthaug + 
                    monthdec + monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                    day_of_weekmon + campaign + 
                    pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                    poutcomefailure + emp.var.rate + 
                    cons.price.idx + cons.conf.idx + nr.employed + 
                    `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_10)
vif(logistic_10)

# Removing monthdec
logistic_11 <- glm(formula = response ~ contactcellular + monthaug + 
                     monthjun + monthmar + monthmay + monthnov + day_of_weekfri + 
                     day_of_weekmon + campaign + 
                     pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx + nr.employed + 
                     `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_11)
vif(logistic_11)

# Removing day_of_weekfri

logistic_12 <- glm(formula = response ~ contactcellular + monthaug + 
                     monthjun + monthmar + monthmay + monthnov +  
                     day_of_weekmon + campaign + 
                     pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx + nr.employed + 
                     `jobblue-collar` + jobservices, family = "binomial", data = train)


summary(logistic_12)
vif(logistic_12)

# Removing jobservices
logistic_13 <- glm(formula = response ~ contactcellular + monthaug + 
                     monthjun + monthmar + monthmay + monthnov +  
                     day_of_weekmon + campaign + 
                     pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure + emp.var.rate + 
                     cons.price.idx + cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)


summary(logistic_13)
vif(logistic_13)

# Removing emp.var.rate
logistic_14 <- glm(formula = response ~ contactcellular + monthaug + 
                     monthjun + monthmar + monthmay + monthnov +  
                     day_of_weekmon + campaign + 
                     pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure +
                     cons.price.idx + cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)


summary(logistic_14)
vif(logistic_14)


# Removing cons.price.idx

logistic_15 <- glm(formula = response ~ contactcellular + monthaug + 
                     monthjun + monthmar + monthmay + monthnov +  
                     day_of_weekmon + campaign + 
                     pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure +
                     cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)


summary(logistic_15)
vif(logistic_15)

# removing monthaug

logistic_16 <- glm(formula = response ~ contactcellular + 
                     monthjun + monthmar + monthmay + monthnov +  
                     day_of_weekmon + campaign + 
                     pdaysContacted_in_first_10days + pdaysContacted_after_10days + 
                     poutcomefailure +
                     cons.conf.idx + nr.employed + 
                     `jobblue-collar`, family = "binomial", data = train)


summary(logistic_16)
vif(logistic_16)

logistic_final <- logistic_16
# All variables are highly significant and there is no more multicollinearity

# Final Model Summary 
summary(logistic_final)

# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.1088  -0.3910  -0.3332  -0.2579   2.9509  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)                    53.5966933  1.4660164  36.559  < 2e-16 ***
#   contactcellular                 0.5191445  0.0607555   8.545  < 2e-16 ***
#   monthjun                        0.2788304  0.0741613   3.760 0.000170 ***
#   monthmar                        0.9053056  0.1180202   7.671 1.71e-14 ***
#   monthmay                       -0.6787391  0.0578903 -11.725  < 2e-16 ***
#   monthnov                       -0.3757194  0.0763975  -4.918 8.75e-07 ***
#   day_of_weekmon                 -0.3309758  0.0543910  -6.085 1.16e-09 ***
#   campaign                       -0.0503430  0.0118187  -4.260 2.05e-05 ***
#   pdaysContacted_in_first_10days  1.3146539  0.0844298  15.571  < 2e-16 ***
#   pdaysContacted_after_10days     1.3156959  0.1667139   7.892 2.98e-15 ***
#   poutcomefailure                -0.5808238  0.0664386  -8.742  < 2e-16 ***
#   cons.conf.idx                   0.0133968  0.0039647   3.379 0.000727 ***
#   nr.employed                    -0.0107236  0.0002865 -37.429  < 2e-16 ***
#   `jobblue-collar`               -0.2201296  0.0583825  -3.770 0.000163 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 20299  on 28831  degrees of freedom
# Residual deviance: 15980  on 28818  degrees of freedom
# AIC: 16008
# 
# Number of Fisher Scoring iterations: 6

#--------------------------------------------------------- 

## Predict Reponses for test data set
predictions_logit <- predict(logistic_final, newdata = test[, -61], type = "response")
summary(predictions_logit)

#--------------------------------------------------------- 

# Finding the optimal probability cutoff

# Let's use the probability cutoff of 50%.
predicted_response <- factor(ifelse(predictions_logit >= 0.50, "yes", "no"))

# Creating confusion matrix for identifying the model evaluation.
conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
conf

# Accuracy : 0.8982          
# Sensitivity : 0.20618         
# Specificity : 0.98605   

perform_fn <- function(cutoff) 
{
  predicted_response <- factor(ifelse(predictions_logit >= cutoff, "yes", "no"))
  conf <- confusionMatrix(predicted_response, test$response, positive = "yes")
  acc <- conf$overall[1]
  sens <- conf$byClass[1]
  spec <- conf$byClass[2]
  out <- t(as.matrix(c(sens, spec, acc))) 
  colnames(out) <- c("sensitivity", "specificity", "accuracy")
  return(out)
}

#---------------------------------------------------------    

# Creating cutoff values from 0.01 to 0.99 for plotting and initiallizing a matrix of 1000 X 4.

s = seq(.01,.99,length=100)

OUT = matrix(0,100,3)


for(i in 1:100)
{
  OUT[i,] = perform_fn(s[i])
} 

#---------------------------------------------------------    

# plotting cutoffs chart for Sensitivity, Specificity & Accuracy 

plot(s, OUT[,1],xlab="Cutoff",ylab="Value",cex.lab=1.5,cex.axis=1.5,ylim=c(0,1),type="l",lwd=2,axes=FALSE,col=2)
axis(1,seq(0,1,length=11),seq(0,1,length=11),cex.lab=1.5)
axis(2,seq(0,1,length=5),seq(0,1,length=5),cex.lab=1.5)
lines(s,OUT[,2],col="darkgreen",lwd=2)
lines(s,OUT[,3],col=4,lwd=2)
box()
legend(0,.50,col=c(2,"darkgreen",4,"darkred"),lwd=c(2,2,2,2),c("Sensitivity","Specificity","Accuracy"))

# Choosing a cutoff of 8%
predicted_response <- factor(ifelse(predictions_logit >= 0.08, "yes", "no"))
conf_final <- confusionMatrix(predicted_response, test$response, positive = "yes")
acc <- conf_final$overall[1]
sens <- conf_final$byClass[1]
spec <- conf_final$byClass[2]

print(c(acc, sens, spec))
# Accuracy Sensitivity Specificity 
# 0.7648106   0.6702586   0.7768150 

## ----------------------------------------------------------------------------------------------------------------
## ---------------------------------------------- Assigment Task 3 ---------------------------------------------- ##
## Create a data frame with the variables prospect ID, actual response, 
## predicted response, predicted probability of response, duration of call in seconds, and cost of call
## While creating the data frame, calculate the cost of call for each prospect in a new column

test$pred_response <- predicted_response
test$pred_prob <- predictions_logit
## Creating a new data frame with required columns
prospect_df <- test[, c("id", "response",  "pred_response", "pred_prob", "duration")]

# Cost of Call
prospect_df$callcost <- 0.033*(prospect_df$duration) + 0.8

# sorting the probabilities in decreasing order 
prospect_df <- prospect_df[order(prospect_df$pred_prob, decreasing = T), ]

# -----------------------------------------------------------------------------------------------------------------
## ---------------------------------------------- Assigment Task 4 ------------------------------------------------ ##
# Find the number of top X% prospects you should target to meet the business objective
# Report the average call duration for targeting the top X% prospects to the CMO

lift <- function(labels , predicted_prob, groups=10) {
  
  if(is.factor(labels)) labels  <- as.integer(as.character(labels ))
  if(is.factor(predicted_prob)) predicted_prob <- as.integer(as.character(predicted_prob))
  helper = data.frame(cbind(labels , predicted_prob))
  helper[,"bucket"] = ntile(-helper[,"predicted_prob"], groups)
  gaintable = helper %>% group_by(bucket)  %>%
    summarise_at(vars(labels ), funs(total = n(),
                                     totalresp=sum(., na.rm = TRUE))) %>%
    mutate(Cumresp = cumsum(totalresp),
           Gain=Cumresp/sum(totalresp)*100,
           Cumlift=Gain/(bucket*(100/groups)))
  return(gaintable)
}


# Create a Table of cumulative gain and lift 
prospect_df$response <- as.factor(ifelse(prospect_df$response=="yes",1,0))
LG = lift(prospect_df$response, prospect_df$pred_prob, groups = 10)

# Business Objective is to target the top 80% of all responders. 
# Total responders = 1392, and 80% of that is 1113. 
# Based on the table of cumulative gain and lift, 
# it can be seen that the 80% target will be met at the 5th decile. 

# 5th decile represents the median. The average cost of targetting these customers is then:
mean(prospect_df[prospect_df$pred_prob > median(prospect_df$pred_prob), ]$duration)
# [1] 264.9139

# Gain Chart 
ggplot(LG, aes(x= bucket, y= Gain)) + 
  geom_path() + 
  labs(title = "Gain Chart",
       xlab="% of total targeted",
       ylab = "% of positive Response")



# -----------------------------------------------------------------------------------------------------------------
## ------------------------------------------ Assigment Task 5 --------------------------------------------------##
# Create a lift chart
# The x-axis contains the number of prospects contacted; 
# the y-axis contains the ratio: response rate using the model/ response rate without using the model

# Lift Chart 
ggplot(LG, aes(x= bucket, y= Cumlift)) + 
  geom_path() + 
  labs(title = "Lift Chart",
       xlab="% of total targeted",
       ylab = "% of positive Response")


# The Cumulative Lift of 3.074 for top two deciles,
# means that when selecting 20% of the records based on the model, 
# one can expect 3.1 times the total number of targets (events) found by randomly 
# selecting 20%-of-records without a model. In terms of customer attrition (churn) model, 
# we can say we can cover 3.1 times the number of attritors by selecting only 20% of the
# customers based on the model as compared to 20% customer selection randomly.

### Analyzing the Charts: Cumulative gains and lift charts are a graphical 
# representation of the advantage of using a predictive model to choose which 
# customers to contact. The lift chart shows how much more likely we are to receive
# respondents than if we contact a random sample of customers. For example,
# by contacting only 10% of customers based on the predictive model we will reach 
# 3 times as many respondents as if we use no model.



