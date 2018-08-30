
t_odi<- read.csv("Course 2/EDA/tendulkar_ODI.csv")
names(t_odi)
str(t_odi)


#removing the asterisk representing not out from runs
t_odi$Runs<- as.factor(gsub("\\*$", "",t_odi$Runs))

t_odi$X4s<- as.factor(t_odi$X4s)
t_odi$X6s<- as.factor(t_odi$X6s)
t_odi$SR<- as.numeric(t_odi$SR)

library(ggplot2)
ggplot(t_odi, aes(x=Runs)) +
  geom_histogram()

ggplot(t_odi, aes(x=X4s)) +
  geom_histogram(stat = "count")
