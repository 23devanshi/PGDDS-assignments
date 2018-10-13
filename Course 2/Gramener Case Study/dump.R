

summary(loan$loan_amnt)
summary(loan$funded_amnt)
summary(loan$funded_amnt_inv)


summary(loan$grade)
summary(sub_grade)


#what is the home ownership background of these people?
summary(loan$home_ownership)
summary(loan$annual_inc)

levels(loan$issue_d)
summary(issue_d)

summary(loan_status)

summary(addr_state)
#why was credit taken?
#majority of the loans were taken for debt consolidation
summary(purpose)
#all application types are individual
summary(loan$application_type)

summary(dti)
summary(delinq_2yrs)

#most accounts have not reported any delinquency in the last 2 years
sum(delinq_2yrs == 0)
ggplot(data= loan, aes(x= sub_grade)) +
  geom_bar()

summary(earliest_cr_line)
ggplot(data= loan, aes(x= earliest_cr_line)) +
  geom_bar()

#is the verification done only over a certain level of income
ggplot(data= loan, aes(x= verification_status, y= annual_inc)) +
  geom_bar(stat = "identity")

#none of the loans have a payment plan in effect
summary(loan$pymnt_plan)
#initial list status is f for all loans
summary(loan$initial_list_status)
#around 50% of the loans were taken with an intent of debt consolidation
summary(loan$purpose)
#where are the loans primarily from?
ggplot(data= loan, aes(x= addr_state)) +
  geom_bar()


#credit inquiries in the last 6 months
summary(inq_last_6mths)

#months since last delinquency is NA for most people
#could be because most people have not defaulted?
sum(is.na(mths_since_last_delinq))

#open credit lines for the borrower
summary(open_acc)
#derogatory public records
summary(pub_rec)
#revolving balance
summary(revol_bal)
summary(revol_util)




