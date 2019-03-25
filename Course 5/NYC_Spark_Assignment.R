## NYC Parking Tickets Assignent

## Group Members
	# Ishan Savio Kerketta
	# Devanshi Kulshreshtha
	# Paromita Sarkar
	# Aniket Mitra 
##---------------------------------------##

## Loading SparkR

spark_path <- '/usr/local/spark'

if (nchar(Sys.getenv("SPARK_HOME")) < 1) {
  Sys.setenv(SPARK_HOME = spark_path)
}

library(SparkR, lib.loc = c(file.path(Sys.getenv("SPARK_HOME"), "R", "lib")))

sparkR.session(master = "yarn", sparkConfig = list(spark.driver.memory = "1g"))

## Extracting Dataset
parking_tickets <- read.df("hdfs:///common_folder/nyc_parking/Parking_Violations_Issued_-_Fiscal_Year_2017.csv", source = "csv", inferSchema = "true", header = "true", na.strings = 'nan')


# Number of rows in original dataset
nrow(parking_tickets)

## 10803028 records

# Before executing hive-sql queries from RStudio, adding jar file in RStudio 
sql("ADD JAR /opt/cloudera/parcels/CDH/lib/hive/lib/hive-hcatalog-core-1.1.0-cdh5.11.2.jar")

# Creating a Temporary View to run SQL queries on
createOrReplaceTempView(parking_tickets, "park_tix_tbl")

# Keeping records (of parking tickets) only of the year 2017
park_tix_2017 <- SparkR::sql("select * from park_tix_tbl where year(`Issue Date`) == 2017")

## Examining the Dataset ##

# Total number of tickets for the Year 2017
nrow(park_tix_2017)

## 5431918 parking tickets have been issued in 2017

#Structure of the Dataset
str(park_tix_2017)

#'SparkDataFrame': 10 variables:                                                 
# $ Summons Number    : num 8478629828 5096917368 1407740258 1413656420 8480309064 1416638830
# $ Plate ID          : chr "66623ME" "FZD8593" "2513JMG" "T672371C" "51771JW" "GLP367"
# $ Registration State: chr "NY" "NY" "NY" "NY" "NY" "NY"
# $ Issue Date        : POSIXct 2017-06-14 2017-06-13 2017-01-11 2017-02-04 2017-01-26 2017-04-30
# $ Violation Code    : int 47 7 78 40 64 20
# $ Vehicle Body Type : chr "REFG" "SUBN" "DELV" "TAXI" "VAN" "SUBN"
# $ Vehicle Make      : chr "MITSU" "ME/BE" "FRUEH" "TOYOT" "INTER" "DODGE"
# $ Violation Precinct: int 14 0 106 73 17 17
# $ Issuer Precinct   : int 14 0 106 73 17 17
# $ Violation Time    : chr "1120A" "0852P" "0015A" "0525A" "0256P" "1232A"

#Summary of the dataset
collect(describe(park_tix_2017))

#Creating a Temporary view to run SQL queries
createOrReplaceTempView(park_tix_2017, "park_tix_2017_tbl")

## Number of unique states from where the cars that got parking tickets came from (without data cleaning)

head(SparkR::sql("select count(distinct `Registration State`) from park_tix_2017_tbl"))

# 65 unique states

## State having maximum entries
head(SparkR::sql("select `Registration State`, count(*) as freq from park_tix_2017_tbl group by `Registration State` order by freq desc"), 5)
# NY has maximum entries

#   State    freq                                                    
#1  NY    4273951
#2  NJ    475825
#3  PA    140286
#4  CT    70403
#5  FL    69468

## Replacing '99' Registration State (erronous value) with state having maximum entries (i.e. NY)
park_tix_2017$`Registration State` <- ifelse(park_tix_2017$`Registration State`==99, 'NY', park_tix_2017$`Registration State`)

#Creating Temporary view again to update changes
createOrReplaceTempView(park_tix_2017, "park_tix_2017_tbl")

## Number of unique states from where the cars that got parking tickets came from (with data cleaning)

head(SparkR::sql("select count(distinct `Registration State`) from park_tix_2017_tbl"))

# 64 unique states

## Aggregation Tasks ##

## How often does each violation code occur?
## Frequency of Top 5 violation codes
head(SparkR::sql("select `Violation code`, count(*) as freq from park_tix_2017_tbl group by `Violation code` order by freq desc"),5)


#Violation code   freq
#1             21 768087
#2             36 662765
#3             38 542079
#4             14 476664
#5             20 319646

#Loading ggplot2 package
library(ggplot2)

# Plotting the data:

df1 <-collect(SparkR::sql("select `Violation code`, count(*) as freq from park_tix_2017_tbl group by `Violation code` order by freq desc limit 6"))

p1 <- ggplot(df1, aes(x = as.factor(`Violation code`),  y = log(freq)))
p1 + geom_bar(stat = "identity", width = 0.5)

## Frequency of Top 5 Vehicle Body Type
head(SparkR::sql("select `Vehicle Body Type`, count(*) as freq from park_tix_2017_tbl group by `Vehicle Body Type` order by freq desc"),5)

#Vehicle Body Type    freq                                                     
#1              SUBN 1883954
#2              4DSD 1547312
#3               VAN  724029
#4              DELV  358984
#5               SDN  194197

## Frequency of Top 5 Vehicle Make

head(SparkR::sql("select `Vehicle Make`, count(*) as freq from park_tix_2017_tbl group by `Vehicle Make` order by freq desc"),5)

#Vehicle Make   freq                                                           
#1         FORD 636844
#2        TOYOT 605291
#3        HONDA 538884
#4        NISSA 462017
#5        CHEVR 356032



## A precinct is a police station that has a certain zone of the city under its command.

## Frequency of Top 6 Violation Precinct after removing Violation Precinct = 0
head(SparkR::sql("select `Violation Precinct`, count(*) as freq from park_tix_2017_tbl where `Violation Precinct` != 0 group by `Violation Precinct` order by freq desc"),6)

#Violation Precinct   freq                                                     
#1                 19 274445
#2                 14 203553
#3                  1 174702
#4                 18 169131
#5                114 147444
#6                 13 125113

# Plotting the data:

df2 <-collect(SparkR::sql("select `Violation Precinct`, count(*) as freq from park_tix_2017_tbl where `Violation Precinct` != 0 group by `Violation Precinct` order by freq desc limit 6"))
 
p2 <- ggplot(df2, aes(x = as.factor(`Violation Precinct`),  y = log(freq)))
p2 + geom_bar(stat = "identity", width = 0.5)

## Frequency of Top 6 Issuer Precinct after removing Issuer Precinct = 0
head(SparkR::sql("select `Issuer Precinct`, count(*) as freq from park_tix_2017_tbl  where `Issuer Precinct` != 0 group by `Issuer Precinct` order by freq desc"),6)

#Issuer Precinct   freq                                                        
#1              19 266961
#2              14 200495
#3               1 168740
#4              18 162994
#5             114 144054
#6              13 122490

## Top 5 Violation code frequency across precinct 19 which have issued the most number of tickets
head(SparkR::sql("select `Issuer Precinct`, `Violation Code`, count(*) as freq from park_tix_2017_tbl where `Issuer Precinct` == 19  group by `Issuer Precinct`, `Violation Code` order by `Issuer Precinct` desc, freq desc"), 5)

#  Issuer Precinct Violation Code  freq                                          
#1              19             46 48445
#2              19             38 36386
#3              19             37 36056
#4              19             14 29797
#5              19             21 28415

## Top 5 Violation code frequency across precinct 14 which have issued the most number of tickets
head(SparkR::sql("select `Issuer Precinct`, `Violation Code`, count(*) as freq from park_tix_2017_tbl where `Issuer Precinct` == 14  group by `Issuer Precinct`, `Violation Code` order by `Issuer Precinct` desc, freq desc"), 5)

#  Issuer Precinct Violation Code  freq                                          
#1              14             14 45036
#2              14             69 30464
#3              14             31 22555
#4              14             47 18364
#5              14             42 10027

## Top 5 Violation code frequency across precinct 1 which have issued the most number of tickets
head(SparkR::sql("select `Issuer Precinct`, `Violation Code`, count(*) as freq from park_tix_2017_tbl where `Issuer Precinct` == 1  group by `Issuer Precinct`, `Violation Code` order by `Issuer Precinct` desc, freq desc"), 5)

#Issuer Precinct Violation Code  freq                                          
#1               1             14 38354
#2               1             16 19081
#3               1             20 15408
#4               1             46 12745
#5               1             38  8535

## Checking for Null values in the dataset

#Checking for Null values in Violation Time
head(SparkR::sql("select count(*) from park_tix_2017_tbl where isNull(`Violation Time`) == 'TRUE'"))
# 16 Null values

#Checking for Null values in Vehicle Make
head(SparkR::sql("select count(*) from park_tix_2017_tbl where isNull(`Vehicle Make`) == 'TRUE'"))
# 38509 Null values

#Removing all Null Values from the dataset
park_tix_2017 <- dropna(park_tix_2017, how = "any")

# Checking number of records
nrow(park_tix_2017)

## 5378917

#Creating Temporary view again to update changes
createOrReplaceTempView(park_tix_2017, "park_tix_2017_tbl")

## The Violation Time field is specified in a strange format. 
## Find a way to make this into a time attribute that you can use to divide into groups.

#Adding 'M' to the string 'Violation Time'
park_tix_2017$M_char <- "M"

park_tix_2017$viol_time_string <- concat(park_tix_2017$`Violation Time`, park_tix_2017$M_char)

#Rectifying time values (changing values like 0015AM to 1215AM) for proper conversion
park_tix_2017$time_12_hr <- ifelse(substr(park_tix_2017$viol_time_string,1,2)=="00", "12", substr(park_tix_2017$viol_time_string,1,2))
park_tix_2017$viol_time_string <- concat(park_tix_2017$time_12_hr, substr(park_tix_2017$viol_time_string,3,6))


#Converting time string to timestamp
park_tix_2017$viol_time <- to_timestamp(park_tix_2017$viol_time_string, format = 'hhmma')

#Extracting hour from timestamp
park_tix_2017$viol_hour <- hour(park_tix_2017$viol_time)

#Dropping unnecessary columns
park_tix_2017$time_12_hr <- NULL
park_tix_2017$M_char <- NULL
park_tix_2017$`Violation Time` <- NULL

#Creating Temporary view again to update changes
createOrReplaceTempView(park_tix_2017, "park_tix_2017_tbl")

##Divide 24 hours into six equal discrete bins of time
df3 <- SparkR::sql("select time_slot, count(*) as num_violation from (select case 
when viol_hour < 4 then 'Bin 1: 00 to 04'
when viol_hour >= 4 and viol_hour <8 then 'Bin 2: 04 to 08'
when viol_hour >= 8 and viol_hour <12 then 'Bin 3: 08 to 12' 
when viol_hour >= 12 and viol_hour <16 then 'Bin 4: 12 to 16'
when viol_hour >= 16 and viol_hour <20 then 'Bin 5: 16 to 20' 
else 'Bin 6: 20 to 24' end as time_slot 
FROM park_tix_2017_tbl)sub
group by time_slot order by time_slot")

head(df3,6)

#        time_slot violations                                                    
#1 Bin 1: 00 to 04     159103
#2 Bin 2: 04 to 08     444965
#3 Bin 3: 08 to 12    2143476
#4 Bin 4: 12 to 16    1828779
#5 Bin 5: 16 to 20     629397
#6 Bin 6: 20 to 24     173197

#Plotting the data
df3_1 <- collect(df3)
p3 <- ggplot(df3_1, aes(x = as.factor(time_slot),  y = (num_violation)))
p3 + geom_bar(stat = "identity", width = 0.5)

#Three most common Violation Codes
#Frequency of Top 3 violation codes
head(SparkR::sql("select `Violation Code`, count(*) as freq from park_tix_2017_tbl group by `Violation code` order by freq desc"),3)


#  Violation code   freq                                                         
#1             21 759775
#2             36 661827
#3             38 540128

## For the three most commonly occurring violation codes, find the most common time of the day 
## (in terms of the bins from the previous part)

#creating binned column 'timeslots'
park_tix_2017$timeslots <- ifelse(park_tix_2017$viol_hour < 4, "0 to 4", 
                                          ifelse(park_tix_2017$viol_hour  >=4 & park_tix_2017$viol_hour  < 8, "4 to 8",
                                                 ifelse(park_tix_2017$viol_hour  >=8 & park_tix_2017$viol_hour  < 12, "8 to 12",
                                                        ifelse(park_tix_2017$viol_hour  >=12 & park_tix_2017$viol_hour  < 16, "12 to 16",
                                                               ifelse(park_tix_2017$viol_hour  >=16 & park_tix_2017$viol_hour  < 20, "16 to 20", "20 to 24")))))

## Most common time of the day for Violation code 21
t1 <- summarize(groupBy(subset(park_tix_2017, park_tix_2017$`Violation code`==21), park_tix_2017$timeslots),
               count = n(park_tix_2017$`Violation code`))
head(arrange(t1, desc(t1$count)))

# timeslots  count                                                              
#1   8 to 12 592582
#2  12 to 16  73917
#3    4 to 8  56723
#4    0 to 4  36134
#5  16 to 20    210
#6  20 to 24    209

# Most common time of the day for Violation code 21 is 8 AM to 12 PM

## Most common time of the day for Violation code 36
t2 <- summarize(groupBy(subset(park_tix_2017, park_tix_2017$`Violation code`==36), park_tix_2017$timeslots),
               count = n(park_tix_2017$`Violation code`))
head(arrange(t2, desc(t2$count)))

#  timeslots  count                                                              
#1   8 to 12 347650
#2  12 to 16 285895
#3    4 to 8  14766
#4  16 to 20  13516

# Most common time of the day for Violation code 36 is 8 AM to 12 PM

## Most common time of the day for Violation code 38
t3 <- summarize(groupBy(subset(park_tix_2017, park_tix_2017$`Violation code`==38), park_tix_2017$timeslots),
               count = n(park_tix_2017$`Violation code`))
head(arrange(t3, desc(t3$count)))

#  timeslots  count                                                              
#1  12 to 16 239909
#2   8 to 12 175779
#3  16 to 20 102581
#4  20 to 24  20311
#5    4 to 8   1252
#6    0 to 4    296

# Most common time of the day for Violation code 38 is 12 PM to 4 PM

## Finding some seasonality in this data

park_tix_2017$issue_month <- month(park_tix_2017$`Issue Date`)

#Loading SparklyR package to use certain commands
library(sparklyr)

# Dividing the year into some number of seasons
park_tix_2017$season <- ifelse(park_tix_2017$issue_month %in% c(9, 10, 11), "Fall", 
                                          ifelse(park_tix_2017$issue_month %in% c(12, 1, 2), "Winter",
                                                 ifelse(park_tix_2017$issue_month %in% c(3, 4, 5), "Spring", "Summer")))

##Creating Temporary view again to update changes
createOrReplaceTempView(park_tix_2017, "park_tix_2017_tbl")

# Finding frequencies of tickets for each season
df4 <- SparkR::sql("select season, count(*) as ticket_freq from park_tix_2017_tbl group by season")
head(df4)

#season ticket_freq                                                            
#1 Spring     2843495
#2 Summer      846864
#3   Fall         889
#4 Winter     1687669

#Plotting the data
df4_1 <- collect(df4)

p4 <- ggplot(df4_1, aes(x = as.factor(season),  y = log(ticket_freq)))
p4 + geom_bar(stat = "identity", width = 0.5)

# Finding the three most common violations for each of these seasons

# Finding the three most common violations for Spring
head(SparkR::sql("select season, `Violation Code`, count(*) as violations from park_tix_2017_tbl
								where season == 'Spring'
								group by season, `Violation Code`
								order by violations desc limit 3"))
# season Violation Code violations                                              
#1 Spring             21     397844
#2 Spring             36     344366
#3 Spring             38     270062

# Finding the three most common violations for Summer
head(SparkR::sql("select season, `Violation Code`, count(*) as violations from park_tix_2017_tbl
								where season == 'Summer'
								group by season, `Violation Code`
								order by violations desc limit 3"))
#season Violation Code violations                                              
#1 Summer             21     126376
#2 Summer             36      96505
#3 Summer             38      83190

# Finding the three most common violations for Fall
head(SparkR::sql("select season, `Violation Code`, count(*) as violations from park_tix_2017_tbl
								where season == 'Fall'
								group by season, `Violation Code`
								order by violations desc limit 3"))
#season Violation Code violations                                              
#1   Fall             46        203
#2   Fall             21        118
#3   Fall             40        114

# Finding the three most common violations for Winter
head(SparkR::sql("select season, `Violation Code`, count(*) as violations from park_tix_2017_tbl
								where season == 'Winter'
								group by season, `Violation Code`
								order by violations desc limit 3"))
#  season Violation Code violations                                              
#1 Winter             21     235437
#2 Winter             36     220956
#3 Winter             38     186869


## The fines collected from all the parking violation constitute a revenue source for the NYC police department.

## Total occurrences of the three most common violation codes
head(SparkR::sql("select `Violation code`, count(*) as freq from park_tix_2017_tbl group by `Violation code` order by freq desc"),3)


#Violation code   freq                                                         
#1             21 759775
#2             36 661827
#3             38 540128

# Visit the website: http://www1.nyc.gov/site/finance/vehicles/services-violation-codes.page 
#It lists the fines associated with different violation codes. 
#They’re divided into two categories, one for the highest-density locations of the city, the other for the rest of the city. 
#For simplicity, take an average of the two.

# Using this information, find the total amount collected for the three violation codes with maximum tickets. 
# State the code which has the highest total collection.

# Violation Code 21
# Street Cleaning: No parking where parking is not allowed by sign, street marking or traffic control device.
# Manhattan 96th St. & below: $65		# All other areas $45
# Average fine = $(65 + 45)/2 = $55

# Violation Code 36
# Exceeding the posted speed limit in or near a designated school zone.
# Manhattan 96th St. & below: $50		# All other areas $50
# Average fine = $50

# Violation Code 38
# Parking Meter -- Failing to show a receipt or tag in the windshield.
# Drivers get a 5-minute grace period past the expired time on parking meter receipts.
# Manhattan 96th St. & below: $65		# All other areas $35
# Average fine = $(65 + 35)/2 = $50

# Using this information, find the total amount collected for the three violation codes with maximum tickets. 
# State the code which has the highest total collection.

#Violation Code	No. of Tickets	Average Fee	Total Revenue
#21		759,775		$55		$41,787,625
#36		661,827		$50		$33,091,350
#38		540,128		$50		$27,006,400

#Violation Code 21 has the highest total collection

## Inference

#Drivers in New York City tend to violate ‘No Parking’ signs the most, out of all traffic violations. 
#NYC Police Department collects maximum revenue for this violation compared to all other traffic violations. 

#This could mean that there are a great number of vehicles in New York City and finding a parking space is difficult. 
#Hence, people tend to take the risk of parking in ‘No Parking’ areas and often commit a traffic violation as a result of this.

#Stopping the Spark Session and Spark Context. 
sparkR.stop()

##---------------------------------------------##
