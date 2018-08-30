popularity<- read.csv("Course 2/EDA/popularity.csv")
str(popularity)
attach(popularity)
max(shares)
quantile(shares, 0.78)

mean(shares)
quantile(shares, seq(0,1, 0.1))

quantile(shares, seq(0,1, 0.01))

popularity<- popularity[which(shares < 10800),]
mean(popularity$shares)
sd(popularity$shares)
