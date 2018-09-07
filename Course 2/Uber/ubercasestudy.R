#loading required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(ggthemes)
library(stringr)

#reading in the data
uber<- read.csv("Course 2/Uber/Uber Request Data.csv")
str(uber)

#looking for missing values in the data
mapply(function(x){sum(is.na(x))} , uber)
#40% of driver Ids are missing
#these could be cases where no cars were available
#drop timestamp is missing- these would be trips that didn't get completed

#counting the number of drivers for whom we have data
length(unique(uber$Driver.id))
#data is available for 301 drivers

summary(uber$Status)
#more than 50% of cases are such where there is an issue in trip completion. 

summary(uber$Pickup.point)

#converting request date and time to posixct format
#data exists in two different formats- these need to be reconciled
Rtime1<- as.POSIXct(uber$Request.timestamp, format= "%d/%m/%Y %H:%M")
Rtime2<- as.POSIXct(uber$Request.timestamp, format= "%d-%m-%Y %H:%M:%S")
Rtime1[is.na(Rtime1)] <- Rtime2[!is.na(Rtime2)]
#checking if the conversion lead to any errors
sum(is.na(Rtime1))
#replacing data in the dataset
uber$Request.timestamp<- Rtime1
uber$Request.day<- format(uber$Request.timestamp, "%d %B")
uber$request.time<- format(uber$Request.timestamp, "%H")

#converting drop date and time to posixct format- only in cases where this data is available
uber.drop.missing<- uber[which(is.na(uber$Drop.timestamp)),]
uber.drop.complete<- uber[which(!is.na(uber$Drop.timestamp)),]
#converting drop time to a workable format
Dtime1<- as.POSIXct(uber.drop.complete$Drop.timestamp, format= "%d/%m/%Y %H:%M")
Dtime2<- as.POSIXct(uber.drop.complete$Drop.timestamp, format= "%d-%m-%Y %H:%M:%S")
Dtime1[is.na(Dtime1)] <- Dtime2[!is.na(Dtime2)]
#checking if the conversion lead to any errors
sum(is.na(Dtime1))
#replacing the column in the dataset
uber.drop.complete$Drop.timestamp <- Dtime1
#appending the two datasets together to have the full dataset
uber<- bind_rows(uber.drop.complete, uber.drop.missing)

#checking if the conversion went through properly
str(uber)

uber$Drop.day<- format(uber$Drop.timestamp, "%d")
uber$drop.time<- format(uber$Drop.timestamp, "%H")

#plotting Status of all requests
plot1<- ggplot(uber, aes(x= Status)) +
  geom_bar(aes(y = (..count..)/sum(..count..)), width = 0.5) + 
  scale_y_continuous(labels=scales::percent) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  labs(title= "Trip Status",x= "Trip Status", y = "Percentage") 

plot1
#how does status vary by pickup point?
plot2<- ggplot(uber, aes(Status, group = Pickup.point)) +
  geom_bar(aes(y = ..prop.., fill = Status)) +
  scale_y_continuous(labels=scales::percent) +
  facet_wrap(~Pickup.point) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  labs(title= "Trip Status by Pick-up Point",x= "Trip Status", y = "Percentage") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

#how does status vary by time of day

#plotting demand versus supply for cars at different times
dd.time<- uber %>%
  group_by(request.time, Status) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = 100 * n/sum(n)) 

plot3<- ggplot(dd.time) + 
  geom_bar(aes(y = n, x = request.time, fill = Status),
           stat="identity") +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  labs(title= "Trip Status by Hour of Day", x= "Hour of the Day", y = "Number of Requests", fill = "Status") + 
  theme(legend.position = "bottom")

plot3

#how does it vary by time of day and pickup point
dd.time.origin<- uber %>%
  group_by(Pickup.point,request.time, Status) %>%
  summarise(n=n()) %>%
  mutate(rel.freq = 100 * n/sum(n)) 

plot4<- ggplot(dd.time.origin) + 
  geom_bar(aes(y = n, x = request.time, fill = Status),
           stat="identity") +
  scale_fill_brewer(palette="Paired") +
  facet_grid(rows = vars(Pickup.point)) +
  theme_minimal() +
  labs(title= "Trip Status by Hour of Day at different Pickup Points", x= "Hour of the Day", y = "Number of Requests", fill = "Status") + 
  theme(legend.position = "bottom")

plot4

#no. of requests made at various hours of the day from various pickup points
requests<- uber %>%
  group_by(request.time, Pickup.point) %>%
  summarise(count= n())

plot5<-ggplot(requests, aes(fill=Pickup.point, y=count, x=request.time)) + 
  geom_bar(position="dodge", stat="identity")+ 
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  labs(title= "Requests made by Hour of Day",x= "Hour of the Day", y = "Requests Made", fill = "Request Origin") + 
  theme(legend.position = "bottom")

#does this show any variation by day?
#no. of requests made at various hours of the day from various pickup points
request.day<- uber %>%
  group_by(request.time, Pickup.point, Request.day) %>%
  summarise(count= n())

plot6<- ggplot(request.day, aes(fill=Pickup.point, y=count, x=request.time)) + 
  geom_bar(position="dodge", stat="identity")+ 
  facet_grid(Request.day~.) +
  scale_fill_brewer(palette="Paired") +
  theme_minimal() +
  labs(title= "Day-wise Variation in Requests made by Hour of Day",x= "Hour of the Day", y = "Requests Made", fill = "Request Origin") + 
  theme(legend.position = "bottom")

#saving the plots
ggsave("Course 2/Uber/plot1.png", plot1)
ggsave("Course 2/Uber/plot2.png", plot2)
ggsave("Course 2/Uber/plot3.png", plot3)
ggsave("Course 2/Uber/plot4.png", plot4)
ggsave("Course 2/Uber/plot5.png", plot5)
ggsave("Course 2/Uber/plot6.png", plot6)

