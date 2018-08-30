grades<- read.csv("Course 2/EDA/grades.csv")
str(grades)
library(stringr)
library(lubridate)
grades$submit_time<- as.POSIXct(grades$submit_time, format = "%m/%d/%y-%H:%M:%S")
deadline1 <- as.POSIXct("Jan 3, 2017 - 23:59:59", format = "%b %d, %Y - %H:%M:%S" )
deadline2 <- as.POSIXct("Jan 9, 2017 - 23:59", format = "%b %d, %Y - %H:%M") 
grades$rollnum<- str_extract(grades$submission, "DDA[0-9]+.zip+|DDA[0-9]+.R|DDA[0-9]+.7z")
grades$ext <- sapply(grades$rollnum, FUN=function(x){unlist(str_split(x, "\\."))[2]})

#grades$delay <- difftime(grades$submit_time, grades$deadline1, units = "secs")

grades$latesub<- ifelse(grades$submit_time > deadline1, 1, 0) 
grades$date<- format(grades$submit_time, "%m/%d/%y")
grades$time<- format(grades$submit_time, "%H")
library(dplyr)

grades %>%
  group_by(date) %>%
  summarise(n= n())

time<- grades %>%
  group_by(time) %>%
  summarise(n= n())

library(ggplot2)
ggplot(data = time, aes(x= time, y = n)) +
  geom_bar(stat = "identity")
