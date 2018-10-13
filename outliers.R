q1 = quantile(loan$loan_amnt,probs = 0.25)
q3 = quantile(loan$loan_amnt, probs = 0.75)
iqr1 = q3-q1
iqr2 = IQR(loan$loan_amnt)

iqr1 == iqr2

upper = q3+1.5*iqr1
lower=  q1-1.5*iqr1

sum(loan$loan_amnt > upper)
sum(loan$loan_amnt < lower)
ggplot(loan, aes(x= loan_amnt, y= annual_inc))+
  geom_point()


qnt <- quantile(loan$annual_inc, probs=c(.25, .75))

H <- 1.5 * IQR(loan$annual_inc)
sum(loan$annual_inc < (qnt[1] - H))
sum(loan$annual_inc > (qnt[2] + H))

y <- x
y[x < (qnt[1] - H)] <- NA
y[x > (qnt[2] + H)] <- NA
x<-y

