library(openxlsx)
churn<- read.xlsx("Course 2/Data Cleaning/churn.xlsx", startRow = 2)
internet<- read.xlsx("Course 2/Data Cleaning/internet.xlsx")
customer<- read.xlsx("Course 2/Data Cleaning/customer.xlsx", startRow = 2)

#total NA values in customer
sum(is.na(customer))
#looking for duplicate rows in NA
customer$gender<- tolower(customer$gender)
customer$Partner<- tolower(customer$Partner)
customer$Dependents<- tolower(customer$Dependents)
#does the data contain any duplicated row
sum(duplicated.data.frame(customer))
#looking for blank spaces that could be NA
sum(customer == " ")
sum(customer == "")

#looking for consistency in the internet dataset
#it does not contain unecessary header rows


#does the data contain any duplicated row
sum(duplicated.data.frame(churn))
#looking for blank spaces that could be NA
sum(internet == " ", na.rm = TRUE)
sum(internet == "", na.rm = TRUE)
sum(duplicated.data.frame(internet))

sapply(internet, function(x) length(which(x == "")))
sapply(internet, function(x) length(which(x == "")))
