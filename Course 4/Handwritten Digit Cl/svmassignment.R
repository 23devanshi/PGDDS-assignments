##########################################################################################
####################################################################################
# Assignment- Support Vector Machine
# Handwritten Digit Classification using MNIST dataset

####################################################################################
### -------------------------- Loading Libraries & Data -------------------------- ####

# install.packages("caret")
# install.packages("kernlab")
# install.packages("dplyr")
# install.packages("readr")
# install.packages("ggplot2")
# install.packages("gridExtra")
# install.packages("scales")
# install.packages("e1071")

library(caret)
library(kernlab)
library(dplyr)
library(readr)
library(ggplot2)
library(gridExtra)
library(scales)
library(e1071)

setwd("D:/Downloads")
train<- read.csv("mnist_train.csv")
test<- read.csv("mnist_test.csv")

####################################################################################
### ------------------------------ Understanding Data  ------------------------- ####

###------------------Training Data ------------------###
#Structure of the dataset
dim(train)
#Training data has 59999 observations across 785 variables
str(train)
#all variables seem to be numeric
#printing first and the last few rows
head(train,3)
tail(train,3)
#There don't seem to be any redundant rows in the data. 
#Exploring the data
summary(train)
names(train)

###------------------Test Data ------------------###
#Structure of the dataset
dim(test)
#Test data has 9999 observations across 785 variables
str(test)
#all variables seem to be numeric
#printing first and the last few rows
head(test,3)
tail(test,3)
#No redundant rows in the test data either. 
#Exploring the data
summary(test)
names(test)

#Understanding more about the independent variables
sum(sapply(train[,-1], function(x) sum(is.integer(x)))) 
sum(sapply(test[-1], function(x) sum(is.integer(x)))) 
# all dependant variables are integers
sapply(train[,-1], min)
sapply(test[,-1], min)
#min seems to be 0 for all variables
#In Pixel Values, 0 represents white
sapply(train[,-1], max)
sapply(test[,-1], max)
#max is capped at 255, though max can also be 0 for some columns
#this is because they represent pixel values 
#0 for white and 255 for black


####################################################################################
### ------------------------------ Data Cleaning ------------------------------ ####

###------------------Column Names ------------------###
#The first column represents the digit that is to be classified. 
#They can be renamed.
colnames(train)[1] <- "digit"
colnames(test)[1] <- "digit"

#are the rest of the variables named the same in the test and train data?
sum(names(test)== names(train))
#turns out the column names for both these are different. In that case, we can rename columns in both these dataframes
#we can name them pixel1 to pixel784
cname <- vector("character", 784) # prepare a container

for (i in 1:784){
  cname[i]<-paste0("pixel", i)
}
colnames(train)[-1] <- cname
colnames(test)[-1] <- cname

###------------------Column Types ------------------###
#digit should be a factor column as this is classification
# Convert digit variable into factor
train$digit <- factor(train$digit)
test$digit <- factor(test$digit)
summary(train$digit)
summary(test$digit)

###------------------Checking for NAs ------------------###
sapply(train, function(x) sum(is.na(x))) 
#It would be difficult to check this across all rows. 
#Summing across the results should be handy. 
sum(sapply(train, function(x) sum(is.na(x)))) 
# There are no missing values in the training dataset
#What about in the test data?
sum(sapply(test, function(x) sum(is.na(x)))) 
# There are no missing values in the test dataset
 
###------------------Duplicate Data ------------------###
# Checking for duplicate data
sum(duplicated(train)) 
# no duplicate data in the training dataset
sum(duplicated(test)) 
# no duplicate data in the test dataset
 
####################################################################################
### ---------------------------- Data Preparation ----------------------------- ###

###------------------Scaling Data ------------------###
#Since pixel values range from 0 to 255, scaling them would require dividing by 255
#This would tell us how each pixel is located, in relation to the black. 
train[,-1] <- train[ ,-1]/255
test[,-1] <- test[ ,-1]/255

###-------------Creating Smaller Sample of Training Data for Model Building ------------###
#Because there are too many observations in the sample dataset, 
#for computational ease, the model should only be run on a fraction of the dataset
#extracting subset of 15% of data for modelling
#this data will be stored in another dataframe called sample
set.seed(100)
sample<- train[sample(nrow(train), round(nrow(train) * 0.15), replace = FALSE),]

str(sample)
#in the sample dataset, the first variable (digit) is an integer

sample$digit<- as.factor(sample$digit)
#to check if this was distributed correctly, 
#we can check the summary of the factor variable digit
summary(sample$digit)
summary(train$digit)

###------------------Data Visualisation for Distribution of Digits ------------------###
#it would make sense to check the percentage distribution to make sure we have sampled correctly
sample_digit<- ggplot(sample, aes(x = digit)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) +
  ggtitle("Sample Data")
  
train_digit <- ggplot(train, aes(x = digit)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) +
  ggtitle("Train Data")

test_digit <- ggplot(test, aes(x = digit)) +  
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=percent) +
  ggtitle("Test Data")

grid.arrange(sample_digit, train_digit, test_digit, ncol= 1, nrow = 3)
#The distribution (relative frequencies) of digits in all 3- train, test and sample is the same. 



####################################################################################
###------------------------ Model Building & Evaluation --------------------------###

#---------------------------------- Linear Kernel -----------------------------------#

## Linear kernel using default parameters
# default takes C= 1
model_linear1 <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "vanilladot", C= 1)
print(model_linear1)
pred_linear<- predict(model_linear1, test)
# Confusion matrix 
caret::confusionMatrix(pred_linear,test$digit)

#overall accuracy: 91.83 %

# How does the model change with a different value of C?
model_linear2 <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "vanilladot", C= 5)
print(model_linear2)
pred2_linear<- predict(model_linear2, test)
# Confusion matrix 
caret::confusionMatrix(pred2_linear,test$digit)
#overall accuracy: 91.43 %

# How does the model change with a different value of C?
model_linear3 <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "vanilladot", C= 10)
print(model_linear3)
pred3_linear<- predict(model_linear3, test)
# Confusion matrix 
caret::confusionMatrix(pred3_linear,test$digit)
#overall accuracy: 91.39 %
#Increasing C reduces accuracy. 

#More efficient value of C would be obtained using parameter tuning

#Trying for different levels of C:
#C= 0.75
model_linear4 <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "vanilladot", C= 0.75)
print(model_linear4)
pred4_linear<- predict(model_linear4, test)
# Confusion matrix 
caret::confusionMatrix(pred4_linear,test$digit)
#overall accuracy: 91.98 %

#C= 0.5
model_linear5 <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "vanilladot", C= 0.5)
print(model_linear5)
pred5_linear<- predict(model_linear5, test)
# Confusion matrix 
caret::confusionMatrix(pred5_linear,test$digit)
#overall accuracy: 92.27 %

#C= 0.25
model_linear6 <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "vanilladot", C= 0.25)
print(model_linear6)
pred6_linear<- predict(model_linear6, test)
# Confusion matrix 
caret::confusionMatrix(pred6_linear,test$digit)
#overall accuracy:  92.79%
# C= 0.1
model_linear7 <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "vanilladot", C= 0.1)
print(model_linear7)
pred7_linear<- predict(model_linear7, test)
# Confusion matrix 
caret::confusionMatrix(pred7_linear,test$digit)
#overall accuracy: 93.1 %

#----------------------------------- RBF Model -----------------------------------#

## Radial kernel using default parameters

model_rbf1 <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "rbfdot")
print(model_rbf1)
pred1_rbf<- predict(model_rbf1, test)
# Confusion matrix 
caret::confusionMatrix(pred1_rbf,test$digit)

#overall accuracy: 95.75
# Using default parameters, this model performs better than linear model

####   Hyperparameter tuning and Cross Validation ####
trainControl <- sampleControl(method="cv", number=5)

# Metric <- "Accuracy" implies our Evaluation metric is Accuracy.
metric <- "Accuracy"

#Expand.grid functions takes set of hyperparameters, that we shall pass to our model.
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05), .C=c(0.1,0.5,1,2) )

#train function takes Target ~ Prediction, Data, Method = Algorithm
#Metric = Type of metric, tuneGrid = Grid of Parameters,
# trcontrol = Our traincontrol method.

fit.svm <- train(digit~., data=sample, method="svmRadial", metric=metric, tuneGrid=grid, trControl=trainControl)

print(fit.svm)
plot(fit.svm)
#The final values used for the model were sigma = 0.025 and C = 2.

model_rbf <- ksvm(digit~ ., data = sample, scale = FALSE, kernel = "rbfdot", C= 2,  kpar=list(sigma=0.05))
print(model_rbf)
pred_rbf<- predict(model_rbf, test)
#confusion matrix 
caret::confusionMatrix(pred_rbf,test$digit)
#accuracy- 96.62

#--------------------------------------------- Polynomial Kernel ----------------------------------------------#

## Polynomial kernel with degree 2, default scale and offset
model_poly1 <- ksvm(digit ~ ., data = sample, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 1))
print(model_poly1)
pred_poly1 <- predict(model_poly1, newdata = test)
caret::confusionMatrix(pred_poly1, test$digit)
# accuracy of 96.2%

## Polynomial kernel with different parameters(scale)
model_poly2 <- ksvm(digit ~ ., data = sample, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = -2, offset = 1))
print(model_poly2)
pred_poly2 <- predict(model_poly2, newdata = test)
caret::confusionMatrix(pred_poly2, test$digit)

# 96.12% accuracy
#reduced accuracy compared to the previous model

## Polynomial kernel with different parameter(offset)
model_poly3 <- ksvm(digit ~ ., data = sample, kernel = "polydot", scaled = FALSE, C = 1, 
                    kpar = list(degree = 2, scale = 1, offset = 10))
print(model_poly3)

pred_poly3 <- predict(model_poly3, test)
caret::confusionMatrix(pred_poly3, test$digit)

# accuract- 96.27
#offset may allow achieving better performance

## Polynomial kernel with different C
model_poly4 <- ksvm(digit ~ ., data = sample, kernel = "polydot", scaled = FALSE, C = 3, 
                    kpar = list(degree = 2, scale = 1, offset = 1))
print(model_poly4)

pred_poly4 <- predict(model_poly4, test)
caret::confusionMatrix(pred_poly4, test$digit)

# 96.2- no change in accuracy

############   Hyperparameter tuning and Cross Validation #####################
grid_poly = expand.grid(C= c(0.01, 0.1, 1, 10), degree = c(1, 2, 3, 4, 5), 
                        scale = c(-100, -10, -1, 1, 10, 100))

poly_trControl= caret::trainControl(method = "cv", number = 2)
fit.poly <- caret::train(digit ~ ., data = sample, metric = "Accuracy", method = "svmPoly",tuneGrid = grid_poly,
                         trControl = poly_trControl, preProcess = NULL)

# printing results of cross validation
print(fit.poly) 
plot(fit.poly)

#The final values used for the model were degree = 2, scale = 1 and C = 0.01.

## Implementing optmised polynomial model 
model_poly <- ksvm(digit ~ ., data = sample, kernel = "polydot", scaled = FALSE, C = 0.01, 
                    kpar = list(degree = 2, scale = 1))
print(model_poly)

pred_poly <- predict(model_poly, test)
confusionMatrix(pred_poly, test$digit)

#This yields an accuracy of 96.2%

################################################ Conclusion ################################################
#Linear model highest accuracy- 93.1 (C= 0.1)
#                       Class: 0  Class: 1  Class: 2  Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity           98.06%    98.68%    92.73%    92.97%    94.60%  87.89%    94.68%  92.70%    86.86%  90.59%
# Specificity           99.46%    99.46%    99.15%    99.03%    98.96%  99.00%    99.33%  99.43%    99.32%  99.19%
# Sensitivity range 86.86% ~ 98.06%. 
# Specificity range 98.96% ~ 99.46%

#Radial model highest accuracy- 96.62 (sigma = 0.025 and C = 2.)
#                       Class: 0  Class: 1  Class: 2  Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity           98.57%    98.85%    96.80%    96.63%    97.35%  95.29%    97.18%  94.45%    96.20%  94.55%
# Specificity           99.78%    99.81%    99.22%    99.49%    99.58%  99.79%    99.78%  99.74%    99.46%  99.60%
# Sensitivity range 94.45% ~ 98.85% . 
# Specificity range 99.22% ~ 99.81% 

#Polynomial model highest accuracy- 96.2 (degree = 2, scale = 1 and C = 0.01.)
#                       Class: 0  Class: 1  Class: 2  Class: 3 Class: 4 Class: 5 Class: 6 Class: 7 Class: 8 Class: 9
# Sensitivity           98.47%    98.77%    95.93%    96.73%    97.56%    94.40%  96.35%  94.84%    94.35%  94.15%
# Specificity           99.65%    99.61%    99.48%    99.54%    99.30%    99.68%  99.66%  99.68%    99.53%  99.66%
# Sensitivity range 94.15% ~ 98.74%. 
# Specificity range 99.30% ~ 99.68%


# Only 15% of the training data was used for model building owing to smaller system capacities. 

#In terms of performance , both the radial model and the polynomial model perform competitively. 
#Accuracy & Sensitivity (across all digits) is marginally higher for the radial model. 
#The polynomial model does better on Specificity (i.e the polynomial model does a better job at knowing what the digit is not)

# Final model is the radial model with the following parameters: sigma = 0.025 and C = 2.
final_model = model_rbf

