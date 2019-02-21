##Install Packages (if not installed)

#install.packages("tidyverse")
#install.packages("forecast")
#install.packages("tseries")
#install.packages("graphics")

#Load Required Packages
library(tidyverse)
library(forecast)
library(tseries)
library(graphics)

## Business Understanding ##

#'Global' Mart is an online superstore
# We have been given timestamped data related to Sales
# We are required to forecast the Sales and Demand (Quanitity) for the next 6 months.

#The store has 7 Markets and 3 Segments
#we are required to make a forecast about
#the most profitable and consistently profitable
#Market-Segment combination


#Data Extraction
superstore <- read.csv("Global Superstore.csv", stringsAsFactors = FALSE, na.strings = c("","NA"))

#General Structure and Summary of the data
str(superstore)
summary(superstore)
nrow(superstore)
View(superstore)

## Data Cleaning and Data Preparation ##

#Checking for duplicates
sum(duplicated(superstore$Row.ID))
#no duplicates

#Number of Orders
length(unique(superstore$Order.ID))

#Check for NA vaues
sapply(superstore, function(x) length(which(is.na(x))))
#Postal code has NA values

#Convert Order Date and Ship Date vaiables to Date format
superstore$Order.Date <- as.Date(superstore$Order.Date, format = "%d-%m-%Y")
superstore$Ship.Date <- as.Date(superstore$Ship.Date, format = "%d-%m-%Y")

#Time frame of Orders 
min(superstore$Order.Date)
max(superstore$Order.Date)

#Convert Discrete variables into Factor
factor_colnames <- c("Segment","City","State","Ship.Mode", "Country", "Market", 
                     "Region", "Category", "Sub.Category", "Order.Priority")

factors <- data.frame(sapply(superstore[factor_colnames], as.factor))
superstore <- superstore[,!(colnames(superstore) %in% factor_colnames)]
superstore <- cbind(superstore,factors)

#Remove unnecessary variables
not_needed <- c("Row.ID","Order.ID","Customer.Name", "Customer.ID", "Product.ID","Postal.Code", "Product.Name")
superstore <- superstore[,!(colnames(superstore) %in% not_needed)]

##Making 21 subsets based on 'Market' and 'Segment'

#creating a column for each market and segment combination
superstore$marketseg <- paste(superstore$Market, "-", superstore$Segment)
superstore$marketseg <- as.factor(superstore$marketseg)

levels(superstore$marketseg)

#Aggregate Sales, Quantity and Profit over Order Date for Monthly values

superstore_monthly <- superstore %>%
  group_by(marketseg, Month = format(superstore$Order.Date,"%B %Y")) %>%
  summarise(Sales = round(sum(Sales),2),
            Quantity = sum(Quantity),
            Profit = round(sum(Profit),2))

View(superstore_monthly)

#Find the 2 most profitable and consistently profitable market segments
profits_marketsegment <- superstore_monthly %>%
  group_by(marketseg) %>%
  mutate(avg_profit = mean(Profit),
         cv_profit = sd(Profit)/mean(Profit)) %>%
  select(marketseg, avg_profit, cv_profit) %>%
  distinct() %>%
  arrange(desc(cv_profit))

View(profits_marketsegment)

#Plotting Average Profit for each Market Segment
ggplot(profits_marketsegment) + 
  geom_bar(aes(x= reorder(marketseg, -avg_profit), y= avg_profit), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x= "Market Segment", y= "Average Profit", title = "Average Profit by Market Segment") 

#Numeric representation of Average profits
profits_marketsegment %>%
  group_by(marketseg) %>%
  summarise(avg_profit) %>%
  arrange(desc(avg_profit))

#Average Profit is highest for EU - Consumer and APAC - Consumer

#Plotting Coefficient of Variation for each Market Segment
ggplot(profits_marketsegment) + 
  geom_bar(aes(x= reorder(marketseg, cv_profit), y= cv_profit), stat = "identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+
  labs(x= "Market Segment", y= "Coefficient of Variation for Profit", title = "Coefficient of Variation for Profit by Market Segment")

#Numeric representation of Coefficient of Variation
profits_marketsegment %>%
  group_by(marketseg) %>%
  summarise(cv_profit) %>%
  arrange(cv_profit)

#Coefficient of Variation is lowest for EU - Consumer and APAC - Consumer

## Hence, the most profitable and consistently profitable Market Segments are:
## EU - Consumer
## APAC - Consumer

##--------------------------------------------------------------------------------------##

##------------------------ For 'EU - Consumer' Market Segment --------------------------##

#Preparing Time Series for Quantity and Sales in the EU - Consumer Market Segment
eu.cons <- subset(superstore_monthly, marketseg == "EU - Consumer")

#Converting the Month into serial number of month
eu.cons$Month <- as.numeric(as.factor(eu.cons$Month))

#Separating Training and Testing data:
eu.cons.train<- eu.cons[1:42,]
eu.cons.test<- eu.cons[43:48,]


##---------------EU - CONSUMER: QUANTITY DEMANDED - MODEL BUILDING & FORECASTING ------------------##

#in declaring this to be a time series, deltat= 1/12 as we have monthly data
eu.cons.qty<- ts(eu.cons.train$Quantity, deltat = 1/12)

##Looking at different components of the TS:

plot(decompose(eu.cons.qty))
## Quantity Demanded shows a sinsoidal trend. 
##There is a seasonal pattern to the data. 

# Plot the timeseries
plot(eu.cons.qty, 
     main= "TS Plot of Quantity (EU- Consumer Segment)", ylab= "Quantity")


#Smoothening the time series- Moving Average Smoothing
w <-1
eu.cons.qty.smooth <- stats::filter(eu.cons.qty, 
                                    filter=rep(1/(2*w+1),(2*w+1)), 
                                    method='convolution', sides=2)


#Smoothing left end of the time series
diff <- eu.cons.qty.smooth[w+2] - eu.cons.qty.smooth[w+1]
for (i in seq(w,1,-1)) {
  eu.cons.qty.smooth[i] <- eu.cons.qty.smooth[i+1] - diff
}

#Smoothing right end of the time series
n <- length(eu.cons.qty)
diff <- eu.cons.qty.smooth[n-w] - eu.cons.qty.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  eu.cons.qty.smooth[i] <- eu.cons.qty.smooth[i-1] + diff
}

#Plotting the smoothed time series as per Moving Average Smoothing
plot(eu.cons.qty, 
     main= "Smoothed TS Plot of Quantity (EU- Consumer Segment)", ylab= "Quantity")
lines(eu.cons.qty.smooth, col="blue", lwd=2)

#Exponential Smoothing taking alpha as 0.3
Holtsmoothedseries <- HoltWinters(eu.cons.qty, alpha=0.3, beta=FALSE, gamma=FALSE)

#Plot smoothed series as per Exponential smoothing
lines(fitted(Holtsmoothedseries)[,1], col="red", lwd=2)

#Displaying the plot's legend
legend("bottomright", c("Raw","MA", "Exp"), col=c("black", "blue", "red"), lwd=2)

##Let us consider the smoothed series obtained by Moving Average method
##as Exponential smoothing results in excessive smoothing

#Looking at the decomposition of smoothed time series
plot(decompose(eu.cons.qty.smooth))

#Storing the timestamps to a separate vector
timevals <- eu.cons.train$Month
timevals.test<- eu.cons.test$Month

#Convert the time series to a dataframe
eu.cons.qty.smoothdf <- as.data.frame(cbind(timevals, as.vector(eu.cons.qty.smooth)))
colnames(eu.cons.qty.smoothdf) <- c('Month', 'Quantity')

#Converting the Month into serial number of month
eu.cons.qty.smoothdf$Month<-as.numeric(eu.cons.qty.smoothdf$Month)
#Converting Quantity into a Numeric vector 
eu.cons.qty.smoothdf$Quantity <- round(as.numeric(as.character(eu.cons.qty.smoothdf$Quantity)),2)
str(eu.cons.qty.smoothdf)

#Plot time series for trend analysis
plot(ts(eu.cons.train$Quantity), main= "Trend of Quantity (EU-Consumer Segment)", ylab= "Quantity")

##Now, let's fit a model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.55*Month) * poly(Month,3) 
            + cos(0.55*Month) * poly(Month,3) + Month, data=eu.cons.qty.smoothdf)

summary(lmfit)

trend <- predict(lmfit, Month= timevals)
summary(trend)
lines(trend, col="green", lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- eu.cons.qty - trend
plot(local_pred, col='red', type = "l", main = "Locally Predictable Series- Quantity (EU Consumer Segment)")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)
#The residual series is strongly stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- eu.cons.test[,c(2,4)]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))

fcast <- global_pred_out
fcast

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(trend),ts(global_pred_out))
plot(ts(eu.cons$Quantity), col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(eu.cons.qty.smooth)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- eu.cons.qty - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
fcast_auto_arima <- predict(autoarima, n.ahead = 12)

MAPE_auto_arima <- accuracy(fcast_auto_arima$pred[1:6],outdata$Quantity)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(fcast_auto_arima$pred))
plot(ts(eu.cons$Quantity), col = "black", main= "Predicted TS- Quantity (EU- Consumer Segment)", ylab= "Quantity")
lines(auto_arima_pred, col = "red")

#Predicting for the next six months
eu.cons.fcast.qty <- data.frame(cbind(fcast_auto_arima$pred[7:12], Month= seq(49:54)))
ggplot(eu.cons.fcast.qty) + geom_line(aes(y= V1, x= Month))+
  labs(x= "Month", y= "Quantity", title = "Quantity Forecast (EU Consumer Segment)") +
  theme_minimal()


##-------------------EU - CONSUMER: SALES - MODEL BUILDING & FORECASTING ----------------------##

#in declaring this to be a time series, deltat= 1/12 as we have monthly data
eu.cons.sales<- ts(eu.cons.train$Sales, deltat = 1/12)

##Looking at different components of the TS:

plot(decompose(eu.cons.sales))
## Sales show a sinsoidal trend. 
##There is a seasonal pattern to the data. 

# Plot the timeseries
plot(eu.cons.sales, main= "TS plot of Sales (EU-Consumer Segment)", ylab= "Sales")

#Smoothening the time series- Moving Average Smoothing
w <-1
eu.cons.sales.smooth <- stats::filter(eu.cons.sales, 
                                      filter=rep(1/(2*w+1),(2*w+1)), 
                                      method='convolution', sides=2)


#Smoothing left end of the time series
diff <- eu.cons.sales.smooth[w+2] - eu.cons.sales.smooth[w+1]
for (i in seq(w,1,-1)) {
  eu.cons.sales.smooth[i] <- eu.cons.sales.smooth[i+1] - diff
}

#Smoothing right end of the time series
n <- length(eu.cons.sales)
diff <- eu.cons.sales.smooth[n-w] - eu.cons.sales.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  eu.cons.sales.smooth[i] <- eu.cons.sales.smooth[i-1] + diff
}

#Plotting the smoothed time series as per Moving Average Smoothing
plot(eu.cons.sales, main= "Smoothed TS plot of Sales (EU-Consumer Segment)", ylab= "Sales")

lines(eu.cons.sales.smooth, col="blue", lwd=2)

#Exponential Smoothing taking alpha as 0.3
Holtsmoothedseries <- HoltWinters(eu.cons.sales, alpha=0.3, beta=FALSE, gamma=FALSE)

#Plot smoothed series as per Exponential smoothing
lines(fitted(Holtsmoothedseries)[,1], col="red", lwd=2)

#Displaying the plot's legend
legend("top", c("Raw","MA", "Exp"), col=c("black", "blue", "red"), lwd=2)

##Let us consider the smoothed series obtained by Moving Average method
##as Exponential smoothing results in excessive smoothing

#Looking at the decomposition of smoothed time series
plot(decompose(eu.cons.sales.smooth))

#Storing the timestamps to a separate vector
#timevals <- eu.cons.train$Month
#timevals.test<- eu.cons.test$Month

#Convert the time series to a dataframe
eu.cons.sales.smoothdf <- as.data.frame(cbind(timevals, as.vector(eu.cons.sales.smooth)))
colnames(eu.cons.sales.smoothdf) <- c('Month', 'Sales')

#Converting the Month into serial number of month
eu.cons.sales.smoothdf$Month<-as.numeric(eu.cons.sales.smoothdf$Month)
#Converting Quantity into a Numeric vector 
eu.cons.sales.smoothdf$Sales <- round(as.numeric(as.character(eu.cons.sales.smoothdf$Sales)),2)
str(eu.cons.sales.smoothdf)

#Plot time series for trend analysis
plot(ts(eu.cons.train$Sales), main= "Trend of Sales (EU-Consumer Segment)", ylab= "Sales")

##Now, let's fit a model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.55*Month) * poly(Month,3) 
            + cos(0.55*Month) * poly(Month,3) + Month, data=eu.cons.sales.smoothdf)

summary(lmfit)

trend <- predict(lmfit, Month= timevals)
summary(trend)
lines(trend, col="green", lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- eu.cons.sales - trend
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)
#The residual series is strongly stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- eu.cons.test[,c(2,3)]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))

fcast <- global_pred_out
fcast

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(trend),ts(global_pred_out))
plot(ts(eu.cons$Sales), col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(eu.cons.sales)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- eu.cons.sales - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
eucons.fcast.sales <- predict(autoarima, n.ahead = 12)

MAPE_auto_arima <- accuracy(eucons.fcast.sales$pred[1:6],outdata$Sales)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(eucons.fcast.sales$pred)[1:6])
plot(ts(eu.cons$Sales), col = "black", main= "Predicted TS- Sales (EU- Consumer Segment)", ylab= "Sales")
lines(auto_arima_pred, col = "red")

#Predicting for the next six months
eu.cons.fcast.sales <- data.frame(cbind(eucons.fcast.sales$pred[7:12], Month= seq(49:54)))
ggplot(eu.cons.fcast.sales) + geom_line(aes(y= V1, x= Month))+
  labs(x= "Month", y= "Sales", title = "Sales Forecast (EU Consumer Segment)") +
  theme_minimal


##----------------------------------------------------------------------------------------##

##------------------------ For 'APAC - Consumer' Market Segment --------------------------##

#Preparing Time Series for Quantity and Sales in the EU - Consumer Market Segment
apac.cons <- subset(superstore_monthly, marketseg == "APAC - Consumer")

#Converting the Month into serial number of month
apac.cons$Month <- as.numeric(as.factor(apac.cons$Month))

#Separating Training and Testing data:
apac.cons.train<- apac.cons[1:42,]
apac.cons.test<- apac.cons[43:48,]

##---------------APAC - CONSUMER: QUANTITY DEMANDED - MODEL BUILDING & FORECASTING ------------------##

#in declaring this to be a time series, deltat= 1/12 as we have monthly data
apac.cons.qty<- ts(apac.cons.train$Quantity, deltat = 1/12)

##Looking at different components of the TS:

plot(decompose(apac.cons.qty))
## Quantity Demanded shows a sinsoidal trend. 
##There is a seasonal pattern to the data. 

# Plot the timeseries
plot(apac.cons.qty, main= "TS plot of Quantity (APAC Consumer Segment)")

#Smoothening the time series- Moving Average Smoothing
w <-1
apac.cons.qty.smooth <- stats::filter(apac.cons.qty, 
                                      filter=rep(1/(2*w+1),(2*w+1)), 
                                      method='convolution', sides=2)


#Smoothing left end of the time series
diff <- apac.cons.qty.smooth[w+2] - apac.cons.qty.smooth[w+1]
for (i in seq(w,1,-1)) {
  apac.cons.qty.smooth[i] <- apac.cons.qty.smooth[i+1] - diff
}

#Smoothing right end of the time series
n <- length(apac.cons.qty)
diff <- apac.cons.qty.smooth[n-w] - apac.cons.qty.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  apac.cons.qty.smooth[i] <- apac.cons.qty.smooth[i-1] + diff
}

#Plotting the smoothed time series as per Moving Average Smoothing
plot(apac.cons.qty, main= "Smoothed TS plot of Quantity (APAC Consumer Segment)")
lines(apac.cons.qty.smooth, col="blue", lwd=2)

#Exponential Smoothing taking alpha as 0.4
Holtsmoothedseries <- HoltWinters(apac.cons.qty, alpha=0.4, beta=FALSE, gamma=FALSE)

#Plot smoothed series as per Exponential smoothing
lines(fitted(Holtsmoothedseries)[,1], col="red", lwd=2)

#Displaying the plot's legend
legend("top", c("Raw","MA", "Exp"), col=c("black", "blue", "red"), lwd=2)

##Let us consider the smoothed series obtained by Moving Average method
##as Exponential smoothing results in excessive smoothing

#Looking at the decomposition of smoothed time series
plot(decompose(apac.cons.qty.smooth))

#Storing the timestamps to a separate vector
timevals <- apac.cons.train$Month
timevals.test<- apac.cons.test$Month

#Convert the time series to a dataframe
apac.cons.qty.smoothdf <- as.data.frame(cbind(timevals, as.vector(apac.cons.qty.smooth)))
colnames(apac.cons.qty.smoothdf) <- c('Month', 'Quantity')

#Converting the Month into serial number of month
apac.cons.qty.smoothdf$Month<-as.numeric(apac.cons.qty.smoothdf$Month)
#Converting Quantity into a Numeric vector 
apac.cons.qty.smoothdf$Quantity <- round(as.numeric(as.character(apac.cons.qty.smoothdf$Quantity)),2)
str(apac.cons.qty.smoothdf)

#Plot the time series for trend analysis
plot(ts(apac.cons.train$Quantity), main= "Trend of Quantity (APAC-Consumer Segment)", ylab= "Quantity")

##Now, let's fit a model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Quantity ~ sin(0.46*Month) * poly(Month,2) 
            + cos(0.345*Month) * poly(Month,2), data=apac.cons.qty.smoothdf)

summary(lmfit)

trend <- predict(lmfit, Month= timevals)
summary(trend)
lines(trend, col="green", lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- apac.cons.qty - trend
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred - fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)
#The residual series is strongly stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- apac.cons.test[,c(2,4)]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))

fcast <- global_pred_out
fcast

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Quantity)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(trend),ts(global_pred_out))
plot(ts(apac.cons$Quantity), col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

autoarima <- auto.arima(apac.cons.qty.smooth)
autoarima
tsdiag(autoarima)
plot(autoarima$x, col="black")
lines(fitted(autoarima), col="red")

#Again, let's check if the residual series is white noise

resi_auto_arima <- apac.cons.qty - fitted(autoarima)

adf.test(resi_auto_arima,alternative = "stationary")
kpss.test(resi_auto_arima)

#Also, let's evaluate the model using MAPE
apaccons.fcast.qty <- predict(autoarima, n.ahead = 12)

MAPE_auto_arima <- accuracy(apaccons.fcast.qty$pred[1:6],outdata$Quantity)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(autoarima),ts(apaccons.fcast.qty$pred))
plot(ts(apac.cons$Quantity), col = "black" , main= "Predicted TS- Quantity (APAC- Consumer Segment)", ylab= "Quantity")
lines(auto_arima_pred, col = "red")


#Predicting for the next six months
apac.cons.fcast.qty <- data.frame(cbind(apaccons.fcast.qty$pred[7:12], Month= seq(49:54)))
ggplot(apac.cons.fcast.qty) + geom_line(aes(y= V1, x= Month))+
  labs(x= "Month", y= "Quantity", title = "Quantity Forecast (APAC Consumer Segment)") +
  theme_minimal()



##-------------------APAC - CONSUMER: SALES - MODEL BUILDING & FORECASTING -----------------##

#in declaring this to be a time series, deltat= 1/12 as we have monthly data
apac.cons.sales<- ts(apac.cons.train$Sales, deltat = 1/12)

##Looking at different components of the TS:

plot(decompose(apac.cons.sales))
## Sales show a sinsoidal trend. 
##There is a seasonal pattern to the data. 

# Plot the timeseries
plot(apac.cons.sales, main= "TS Plot os Sales (APAC Consumer)")

#Smoothening the time series- Moving Average Smoothing
w <-1
apac.cons.sales.smooth <- stats::filter(apac.cons.sales, 
                                        filter=rep(1/(2*w+1),(2*w+1)), 
                                        method='convolution', sides=2)


#Smoothing left end of the time series
diff <- apac.cons.sales.smooth[w+2] - apac.cons.sales.smooth[w+1]
for (i in seq(w,1,-1)) {
  apac.cons.sales.smooth[i] <- apac.cons.sales.smooth[i+1] - diff
}

#Smoothing right end of the time series
n <- length(apac.cons.sales)
diff <- apac.cons.sales.smooth[n-w] - apac.cons.sales.smooth[n-w-1]
for (i in seq(n-w+1, n)) {
  apac.cons.sales.smooth[i] <- apac.cons.sales.smooth[i-1] + diff
}

#Plotting the smoothed time series as per Moving Average Smoothing
plot(apac.cons.sales, main= "Smoothed TS Plot os Sales (APAC Consumer)")
lines(apac.cons.sales.smooth, col="blue", lwd=2)

#Exponential Smoothing taking alpha as 0.4
Holtsmoothedseries <- HoltWinters(apac.cons.sales, alpha=0.4, beta=FALSE, gamma=FALSE)

#Plot smoothed series as per Exponential smoothing
lines(fitted(Holtsmoothedseries)[,1], col="red", lwd=2)

#Displaying the plot's legend
legend("top", c("Raw","MA", "Exp"), col=c("black", "blue", "red"), lwd=2)

##Let us consider the smoothed series obtained by Moving Average method
##as Exponential smoothing results in excessive smoothing

#Looking at the decomposition of smoothed time series
plot(decompose(apac.cons.sales.smooth))

#Storing the timestamps to a separate vector
#timevals <- apac.cons.train$Month
#timevals.test<- apac.cons.test$Month

#Convert the time series to a dataframe
apac.cons.sales.smoothdf <- as.data.frame(cbind(timevals, as.vector(apac.cons.sales.smooth)))
colnames(apac.cons.sales.smoothdf) <- c('Month', 'Sales')

#Converting the Month into serial number of month
apac.cons.sales.smoothdf$Month<-as.numeric(apac.cons.sales.smoothdf$Month)
#Converting Sales into a Numeric vector 
apac.cons.sales.smoothdf$Sales <- round(as.numeric(as.character(apac.cons.sales.smoothdf$Sales)),2)
str(apac.cons.sales.smoothdf)

#Plot the time series for trend analysis
plot((ts(apac.cons.train$Sales)), main= "Trend of Sales (APAC-Consumer Segment)", ylab= "Sales")

##Now, let's fit a model with trend and seasonality to the data
#Seasonality will be modeled using a sinusoid function

lmfit <- lm(Sales ~ sin(0.566*Month) * poly(Month,3) 
            + cos(0.33*Month) * poly(Month,2), data=apac.cons.sales.smoothdf)

summary(lmfit)

trend <- predict(lmfit, Month= timevals)
summary(trend)
lines(trend, col="green", lwd=2)

#Now, let's look at the locally predictable series
#We will model it as an ARMA series

local_pred <- apac.cons.sales - trend
plot(local_pred, col='red', type = "l")
acf(local_pred)
acf(local_pred, type="partial")
armafit <- auto.arima(local_pred)

tsdiag(armafit)
armafit

#We'll check if the residual series is white noise

resi <- local_pred-fitted(armafit)

adf.test(resi,alternative = "stationary")
kpss.test(resi)
#The residual series is strongly stationary

#Now, let's evaluate the model using MAPE
#First, let's make a prediction for the last 6 months

outdata <- apac.cons.test[,c(2,3)]
timevals_out <- outdata$Month

global_pred_out <- predict(lmfit,data.frame(Month = timevals_out))

fcast <- global_pred_out
fcast

#Now, let's compare our prediction with the actual values, using MAPE

MAPE_class_dec <- accuracy(fcast,outdata$Sales)[5]
MAPE_class_dec

#Let's also plot the predictions along with original values, to
#get a visual feel of the fit

class_dec_pred <- c(ts(trend),ts(global_pred_out))
plot(ts(apac.cons$Sales), col = "black")
lines(class_dec_pred, col = "red")

#So, that was classical decomposition, now let's do an ARIMA fit

apac.autoarima.sales <- auto.arima(apac.cons.sales)
apac.autoarima.sales
tsdiag(apac.autoarima.sales)
plot(apac.autoarima.sales$x, col="black")
lines(fitted(apac.autoarima.sales), col="red")

#Again, let's check if the residual series is white noise

apac.resi.sales.arima <- apac.cons.sales - fitted(apac.autoarima.sales)

adf.test(apac.resi.sales.arima,alternative = "stationary")
kpss.test(apac.resi.sales.arima)

#Also, let's evaluate the model using MAPE
apaccons.fcast.sales <- predict(apac.autoarima.sales, n.ahead = 12)

MAPE_auto_arima <- accuracy(apaccons.fcast.sales$pred[1:6],outdata$Sales)[5]
MAPE_auto_arima

#Lastly, let's plot the predictions along with original values, to
#get a visual feel of the fit

auto_arima_pred <- c(fitted(apac.autoarima.sales),ts(apaccons.fcast.sales$pred))
plot(ts(apac.cons$Sales), col = "black", main= "Predicted TS- Sales (APAC- Consumer Segment)", ylab= "Sales")

lines(auto_arima_pred, col = "red")


#Predicting for the next six months
apac.cons.fcast.sales <- data.frame(cbind(apaccons.fcast.sales$pred[7:12], Month= seq(49:54)))
ggplot(apac.cons.fcast.sales) + geom_line(aes(y= V1, x= Month))+
  labs(x= "Month", y= "Sales", title = "Sales Forecast (APAC Consumer Segment)") +
  theme_minimal()

##----------------------------------------------------------------------------------------------##

