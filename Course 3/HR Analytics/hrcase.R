
esd<- read.csv("Course 3/HR Analytics/employee_survey_data.csv", stringsAsFactors = F)
msd<- read.csv("Course 3/HR Analytics/manager_survey_data.csv", stringsAsFactors = F)
gd<- read.csv("Course 3/HR Analytics/general_data.csv", stringsAsFactors = F)

#working with employee survey data
summary(esd)
length(unique(esd$EmployeeID))

#working with in time and out time
in_time<- read.csv("Course 3/HR Analytics/in_time.csv", stringsAsFactors = F)
out_time<- read.csv("Course 3/HR Analytics/out_time.csv", stringsAsFactors = F)
#before calculating the time difference, it should be known whether colnames in both files are similar
#(i.e. do they refer to the same date)
sum(colnames(in_time) == colnames(out_time))
#this returns true. 
#str(in_time)

#creating a dataframe similar to in_time (same column names)
#column values will be replaced by time diff (hours worked in a day)
hours<- in_time
hours[,-1] <- NA
hours[,3] <- difftime(strptime(out_time[,3], format = "%Y-%m-%d %H:%M:%S"),
                      strptime(in_time[,3], format = "%Y-%m-%d %H:%M:%S"),units="hours")

#certain columns contain only NAs
rows<- c(2,11,19,47,88, 143,187,198,224,225,226,258)
for (i in setdiff(1:262, rows)){
  hours[,i] <- difftime(strptime(out_time[,i], format = "%Y-%m-%d %H:%M:%S"),
                        strptime(in_time[,i], format = "%Y-%m-%d %H:%M:%S"),units="hours")
  
}