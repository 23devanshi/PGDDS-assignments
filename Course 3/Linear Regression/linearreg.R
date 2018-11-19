#loading required packages
library(MASS)
library(car)
library(corrplot)
library(ggplot2)
library(dplyr)
library(gridExtra)
library(MASS)
#reading in the data
cars<- read.csv("Course 3/Linear Regression/CarPrice_Assignment.csv", stringsAsFactors = F)
str(cars)

#understanding variables
names(cars)

##------------------- DATA PREPARATION -------------------##

###------- Duplicate Values ----------###
#checking if the rows are unique
nrow(unique(cars))
#all rows are unique

###-------- Missing Values ----------###
#checking for missing values
sum(is.na(cars))
#there are no missing values

###---------- Addressing Data Quality ------------###
####------ Car Name --------####
#separating name of car maker from CarName
cars$company<- sapply(cars$CarName, function (x) {strsplit(x, "[[:space:]]")[[1]][1]})
unique(cars$company)
#there seem to be a lot of different spellings for the same car company
#this can be fixed
cars$company[cars$company == "maxda"] <- "mazda"
cars$company[cars$company == "Nissan"] <- "nissan"
cars$company[cars$company == "porcshce"] <- "porsche"
cars$company[cars$company == "toyouta"] <- "toyota"
cars$company[cars$company == "vokswagen" |cars$company == "vw" ] <- "volkswagen"

# Tidying cylindernumber variables
#cylindernumber has levels like 'eight', 'five'
#cylinder number is effectively a numerical variable 
#unique(cars$cylindernumber)

###---------- Outliers ------------###
#checking and treating for outliers
#writing a function to plot quantiles to identify jumps
qtplot<- function(x){
  plot(quantile(x, seq(0, 1, 0.01)))  
}
qtplot(cars$wheelbase)
#wheel base shows no notable jump
qtplot(cars$carlength)
#no notable jump
qtplot(cars$carwidth)
#no notable jump
qtplot(cars$carheight)
#no notable jump
qtplot(cars$enginesize)
#no notable jump
qtplot(cars$boreratio)
quantile(cars$boreratio, seq(0, 1, 0.01))
#bore ratio has jump at the 99th percentile- not considerable 
qtplot(cars$stroke)
quantile(cars$stroke, seq(0, 1, 0.01))
#jump before the 2nd percentile
#dropping variables is not an option- we have too few observations
#these values can be capped 
cars$stroke[which(cars$stroke < 2.64)] <- 2.64
qtplot(cars$compressionratio)
#huge jump around the 90th percentile
quantile(cars$compressionratio, seq(0, 1, 0.01))
#reassigning all values above the 90th percentile to 10.9400
cars$compressionratio[which(cars$compressionratio > 10.9400)] <- 10.9400
qtplot(cars$horsepower)
quantile(cars$horsepower, seq(0, 1, 0.01))
cars$horsepower[which(cars$horsepower > 207.00)] <- 207.00
#99th percentile
qtplot(cars$peakrpm)
quantile(cars$peakrpm, seq(0, 1, 0.01))
cars$peakrpm[which(cars$peakrpm > 6000)] <- 6000
#99th percentile
qtplot(cars$citympg)
quantile(cars$citympg, seq(0, 1, 0.01))
cars$citympg[which(cars$citympg > 38.00)] <- 38.00
qtplot(cars$highwaympg)
#no significant jumps
qtplot(cars$price)
#no significant jumps

##------------------- Exploratory Data Analysis -------------------##

#Understanding the Distribubtion of Price
summary(cars$price)
#The Median Price of cars is 10295
#Considerable jump is seen between the 3rd Quartile and the Maximum Value- 
#This could be because of presence of outliers in the price value
#A plot would help see this better
boxplot(cars$price)
#There are quite a few "outliers" in teh data
#This could be becausethere are several cars in the luxury segment 
#that attract higher prices than the small-medium consumer segment. 
#Notably, there are no outliers in the lower end.

#plotting all numerical variables along with price
#numeric variables are: 
num_cars<- cars[,c(2, 10:14, 17, 19:26)]
res<- cor(num_cars)
corrplot(res, method = "color")
#price is correlated with several variables. 
#wheelbase, carlength, carwidth, curbweight, enginesize and horsepower

#mpg is negatively related to most variables, 
#except compression ratio (weight and horsepower in particular)

#checking effect of categorical variables
charvar <- c("fueltype","aspiration", "doornumber", "carbody", "drivewheel", 
	"enginelocation", "enginetype", "cylindernumber", "fuelsystem", "company")
cars[,charvar] <- lapply(cars[,charvar], function(x) as.factor(x))

#plotting all variables with two levels

box_2<- function(df, x, y){
  aaa <- enquo(x)
  bbb <- enquo(y)
  p<- ggplot(df, mapping = aes_string(x= aaa, y= bbb)) + geom_boxplot() 
  plot(p)}

#plotting all graphs with 2 levels
#step 1- list out all variables having excatly two levels
#there are 4 such variables- fuel type, aspiration, doors and enginelocation
level_2<- sapply(cars, function (x){length(levels(x)) == 2})
grobs_2<- lapply(cars[level_2], box_2, df= cars, y= price)
gridExtra::grid.arrange( grobs = grobs_2, ncol = 4)
#engine location looks like a very important determinant of car price

#plotting variables with multiple levels
#there are 7 such variables
# there are several other variables that are categorical with multiple levels
level_m<- sapply(cars, function (x){length(levels(x)) > 2})
grobs_m<- lapply(cars[level_m], box_2, df= cars, y= price)
gridExtra::grid.arrange( grobs = grobs_m, nrow = 3)

#porsche and jaguar are priced significantly higher, 
#while some models in bmw are also priced higher


#creating dummy variables for categorical variables
#treating all categorical variables with two levels collectively
#step 2 checking their summary
summary(cars[level_2])
#step 3- reassigning levels in only these variables
cars[level_2] <- lapply(cars[level_2], function(x) {
  levels(x) <- c(1,0)
  x
})

#step 4- checking their summary
summary(cars[level_2])

#for fuel type, 1 represents diesel
#for aspiration, 1 represents std
#for doornumber, 1 represents four
#for engine location, 1 represents front

#there are 7 other categorical variables with multiple levels
#carbody
#it has 5 levels- convertible, hardtop, hatchback, sedan, wagon
#creating a dataframe with index number of this variable
dummy_7<- data.frame(model.matrix( ~carbody, data = cars))
dummy_7<- dummy_7[,-1]

#drive wheel
#it has 3 levels
dummy_8<- data.frame(model.matrix( ~drivewheel, data = cars))
dummy_8<- dummy_8[,-1]

#engine type
#it has 7 levels
dummy_15<- data.frame(model.matrix( ~enginetype, data = cars))
dummy_15<- dummy_15[,-1]

#cylinder number
#it has 7 levels
dummy_16<- data.frame(model.matrix( ~cylindernumber, data = cars))
dummy_16<- dummy_16[,-1]

#fuel system
#1bbl 2bbl 4bbl  idi  mfi mpfi spdi spfi 
dummy_18<- data.frame(model.matrix( ~fuelsystem, data = cars))
dummy_18<- dummy_18[,-1]

#company has 28 levels
dummy_27<- data.frame(model.matrix( ~company, data = cars))
dummy_27<- dummy_27[,-1]

#replacing the categorical variables with these multiple dummy variables
cars_1<- cbind(cars[,-c(7, 8, 15, 16, 18, 27)], dummy_7, dummy_8, 
	dummy_15, dummy_16, dummy_18, dummy_27)

#for efficiency, the temporarily created dataframes should be removed
rm(num_cars, dummy_7, dummy_8, dummy_15, dummy_16, dummy_18, dummy_27)

##------------------- Model Building -------------------##
#removing variables that aren't relevant to model building
cars_1<- cars_1[, -c(1,3)]
#splitting the data to train & test variables
set.seed(100)
train<- sample(1:nrow(cars_1), 0.7*nrow(cars_1))
cars_train<- cars_1[train,]
cars_test<- cars_1[-train,]

#Model 1
#the size of the car would be an important dynamic in determining price.
#heavier cars- more horsepower /engine size should correspond monotonically with price
#mpg was highly correlated with price, so it should be a good predictor
#engine location also corresponded with difference in median price
model_1<- lm(price ~ carlength + carwidth + carheight + curbweight + enginesize + 
               citympg + highwaympg + horsepower + enginelocation, 
             data = cars_train)
summary(model_1)
#model has Adjusted R-squared of 0.8871, but many variables are not significant 
#mpg (city or highway) does not turn out to be significant 
#horepower is not significant
#how well does this model perform on the test data?
cars_test$price_1<- predict(model_1, cars_test[,-19])
cor(cars_test$price,cars_test$price_1)^2
#0.748


#choosing model variables as those most collinear with price
model_2<- lm(price ~ carwidth + curbweight + enginesize + citympg + 
               highwaympg + horsepower + enginelocation, 
	data = cars_train)
summary(model_2)
#model has a high Adjusted R-squared (0.8835), 
#either mpg does not turn out to be significant 
#horepower is not significant
vif(model_2)
#how well does this model perform on the test data?
cars_test$price_2<- predict(model_2, cars_test[,-19])
cor(cars_test$price,cars_test$price_2)^2
#0.758

#city mpg and highway mpg are highly correlated- 
#it would make sense to drop one and rerun the model
#also removing horsepower as it is not significant
model_3<- lm(price ~ carwidth + curbweight + enginesize 
             + citympg + enginelocation, 
	data = cars_train)
summary(model_3)
#there is mild change in adjusted R-square (0.8843)
#citympg is still insignificant, 
#curbweight has also reduced in significance
vif(model_3)

cars_test$price_3<- predict(model_3, cars_test[,-19])
cor(cars_test$price,cars_test$price_3)^2
#0.751

model_4<- lm(price ~ carwidth + curbweight + enginesize + 
               enginelocation, 
	data = cars_train)

summary(model_4)
#0.8824 is the adjusted R- square for this model
#but all variables are significant now
#checking this model for multicollinearity
vif(model_4)
#the vif of all but 1 coefficient is questionable
cars_test$price_4<- predict(model_4, cars_test[,-19])
cor(cars_test$price,cars_test$price_4)^2
#0.747

#removing curbweight- based on vif and significance
model_5<- lm(price ~ carwidth + enginesize + enginelocation, 
	data = cars_train)
summary(model_5)
#adjusted R-square is now 0.8759
#all variables are significant
#need to check this model for multicollinearity
vif(model_5)
#engine size has a vif value close to 3
cars_test$price_5<- predict(model_5, cars_test[,-19])
cor(cars_test$price,cars_test$price_5)^2
#0.7384727

#removing engine size and runnig the model again
model_6<- lm(price ~ carwidth + enginelocation, 
             data = cars_train)
summary(model_6)
#Adjusted R-squaree drops considerably 0.7781
#all variables are significant
#checking for multicollinearity
vif(model_6)
#vif is at an acceptable level
#how well does the model perform on test data?
cars_test$price_6<- predict(model_6, cars_test[,-19])
cor(cars_test$price,cars_test$price_6)^2
#this model performs very poorly on test data - 0.524

#alternatively, step AIC can be used to build this model
model_7<- lm(price~., data = cars_train)
summary(model_7)
#very few variables are significant
#the adjusted R-square of this model is extremely high- 0.9694
cars_test$price_7<- predict(model_7, cars_test[,-19])
cor(cars_test$price,cars_test$price_7)^2
#0.8462193

step<- stepAIC(model_7, trace= FALSE)
step$anova
#from the AIC, the best model will be:
model_8<- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
               enginesize + boreratio + stroke + horsepower + peakrpm + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohc + 
               enginetypeohcf + enginetyperotor + cylindernumberfive + cylindernumberthree + 
               fuelsystem2bbl + fuelsystemmpfi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymazda + 
               companymitsubishi + companynissan + companyplymouth + companyrenault + 
               companysaab + companytoyota + companyvolkswagen, data= cars_train)
summary(model_8)
#this model has an adjusted R-square of 0.9726- higher than the model with all variables
#but there are several variables that are not significant
vif(model_8)
#how well does this model perform on the test data?
cars_test$price_8<- predict(model_8, cars_test[,-19])
cor(cars_test$price,cars_test$price_8)^2
#[1] 0.835


#based on vif and significance, horsepower can be removed
model_9<- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
               enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
               carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
               enginetypedohcv + enginetypel + enginetypeohc + enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               fuelsystem2bbl + fuelsystemmpfi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymazda + 
               companymitsubishi + companynissan + companyplymouth + 
               companyrenault + companysaab + companytoyota + 
               companyvolkswagen, data= cars_train)
summary(model_9)
#this model has an adjusted R-square of 0.9724
#more significant variables than in the previous model
vif(model_9)
#engine size has vif of 21 (highest) it is significant

cars_test$price_9<- predict(model_9, cars_test[,-19])
cor(cars_test$price,cars_test$price_9)^2
# 0.8349543



#enginetypeohc has vif around 6 and is not significant
model_10<- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
               enginesize + boreratio + stroke + peakrpm + 
               carbodyhardtop + carbodyhatchback + carbodysedan + carbodywagon + 
               drivewheelrwd + enginetypedohcv + enginetypel + enginetypeohcf + 
               enginetyperotor + cylindernumberfive + cylindernumberthree + 
               fuelsystem2bbl + fuelsystemmpfi + companybmw + companybuick + 
               companydodge + companyhonda + companyjaguar + companymazda + 
               companymitsubishi + companynissan + companyplymouth + 
               companyrenault + companysaab + companytoyota + companyvolkswagen, 
             data= cars_train)
summary(model_10)
#this model has an adjusted R-square of 0.9722
#only 3 variables are insignificant- 
vif(model_10)

cars_test$price_10<- predict(model_10, cars_test[,-19])
cor(cars_test$price,cars_test$price_10)^2
#0.8408669

#fuelsystemmpfi has vif around 5 and is not significant

model_11<- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                cylindernumberfive + cylindernumberthree + fuelsystem2bbl + 
                companybmw + companybuick + companydodge + companyhonda + 
                companyjaguar + companymazda + companymitsubishi + 
                companynissan + companyplymouth + companyrenault + 
                companysaab + companytoyota + companyvolkswagen, data= cars_train)
summary(model_11)
#Adjusted R-square is now 0.9722
#only 2 variables are insignificant at 95% confidence interval
vif(model_11)
#how well does this model perform on test data?
cars_test$price_11<- predict(model_11, cars_test[,-19])
cor(cars_test$price,cars_test$price_11)^2
#0.8405423


#fuelsystem2bbl has a vif of 3 and is also insignificant
model_12<- lm(price ~ aspiration + enginelocation + carwidth + curbweight + 
                   enginesize + boreratio + stroke + peakrpm + carbodyhardtop + 
                   carbodyhatchback + carbodysedan + carbodywagon + drivewheelrwd + 
                   enginetypedohcv + enginetypel + enginetypeohcf + enginetyperotor + 
                   cylindernumberfive + cylindernumberthree + companybmw + companybuick + 
                   companydodge + companyhonda + companyjaguar + companymazda + 
                   companymitsubishi + companynissan + companyplymouth + companyrenault + 
                   companysaab + companytoyota + companyvolkswagen, data= cars_train)
summary(model_12)
#adjusted R-square is now 0.9723
vif(model_12)
#how well does this model perform on test data?
cars_test$price_12<- predict(model_12, cars_test[,-19])
cor(cars_test$price,cars_test$price_12)^2
#0.8394599

#all variables are significant

#curbweight is consistently getting a high vif


model_13<- lm(price ~ aspiration + enginelocation + carwidth + enginesize + 
                boreratio + stroke + peakrpm + carbodyhardtop + carbodyhatchback + 
                carbodysedan + carbodywagon + drivewheelrwd + enginetypedohcv + 
                enginetypel + enginetypeohcf + enginetyperotor + cylindernumberfive + 
                cylindernumberthree + companybmw + companybuick + companydodge + 
                companyhonda + companyjaguar + companymazda + companymitsubishi + 
                companynissan + companyplymouth + companyrenault + companysaab + 
                companytoyota + companyvolkswagen, data= cars_train)
summary(model_13)
vif(model_13)
#adjusted R-square is now 0.9707  
#using it on the test data set to see how well this model fits
cars_test$price_13<- predict(model_13, cars_test[,-19])
cor(cars_test$price,cars_test$price_13)^2
#0.8329041
#compared to the previous model, ithas a lower accuracy on both 
#the training and the test dataset and 
# fewer significant variables

#model 12 would be the better choice of model
#We are able to predict with 97.23% accuracy in the training data
#and 83% accuracy in the test data set
##------------------   Interpretation of Model ----------------  ##
#car width, curb weight, engine size and peakrpm are positively related to car price
## -------- Car Body
#among different car bodies, convertibles attract the highest price. 
#wagon and hatchback fetch the lowest prices
#-------- Drive Wheel
# Rear wheel drive vehicles attract lower prices than its alternatives
#-------- Engine Type
#Engine types rotor and dohcv attract higher car prices
#L and OHCF  type engines are correlated with lower price of cars
#-------- Cylinder Number
#Cars with 3 cylinders attract higher price than cars with not
#Having five cylinders negatively impacts price of car
#-------- Company
#certain car companies attract a higher price- possibly due to them being in the luxury segment
#among these- bmw, buick and jaguar
#certain car companies cater to a broader segment and make small to mid size cars
#the prices they command are thus lower
#among these- dodge, honda, maza, mitsubishi, nissan, plymouth, renault, saab, toyota and wv