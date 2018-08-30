
## Function for package installation 
pkg <- function(x)
{
  if (!require(x,character.only = TRUE))
  {
    install.packages(x,dep=TRUE)
    if(!require(x,character.only = TRUE)) stop("Package not found")
  }
}

pkg("dplyr")
pkg("stringr")
pkg("tidyr")
#pkg("ISLR")
pkg("countrycode")

#Checkpoints - Part 1

#read the companies dataset
#fill = T is needed because data does not exist for all companies ----- Do we need this line ?
companies <- read.delim("Course 1/Investmentcasestudy/companies.txt", sep="\t", stringsAsFactors=F, header=T)
#66368 
str(companies)

#read the funding rounds details dataset
rounds2 <- read.csv("Course 1/Investmentcasestudy/rounds2.csv", stringsAsFactors=F)
#114949
str(rounds2)

#Addressing Data quality issues

# Convert all permalinks of both the files into lowercase/uppercase.
# Permalink is a unique id for each company
# calculate unique permalinks in rounds2 and companies files.

rounds2$company_permalink<- tolower(rounds2$company_permalink)
companies$permalink<- tolower(companies$permalink)

#Table 1.1 #Checkpoint 1 ------------------------------------------

#1) unique companies present in rounds2
length(unique(rounds2$company_permalink))
# 66368 

#2) unique companies present in companies 
length(unique(companies$permalink))
# 66368 

#3)'permalink' present in companies dataset is unique for each company

#4)
#Are there any companies in the 'rounds2' file which are not present in 'companies' dataset?
#using setdiff from dplyr package to count intersection b/w both columns
#have added unique() because a company may have gone through multiple funding rounds
length(setdiff(unique(rounds2$company_permalink), unique(companies$permalink)))
# 0 

#5) Merging two data frames
master_frame<-merge(companies, rounds2, by.x = "permalink", by.y="company_permalink")

#Table 2.1 #Checkpoint 2 (Funding Type Analysis) -------------------

#Total number of NA values present in column raised_amount_usd--- 
sum(is.na(master_frame$raised_amount_usd))
sum(is.na(master_frame$raised_amount_usd))/nrow(master_frame)
# 17% missing values

#funding types available in the dataset can be found by
summary(factor(master_frame$funding_round_type))

# Spark Funds is interested in four kinds of funding: venture, angel, seed, private equity
#Excluding NA for average calculation
fundingtype <- master_frame %>%
               filter(funding_round_type == "venture" | 
                      funding_round_type == "angel" |
                      funding_round_type == "seed" |
                      funding_round_type == "private_equity") %>%
               group_by(funding_round_type) %>%
               summarise(avg_investment = mean(raised_amount_usd, na.rm = T))

#Average Investment in million
fundingtype$avg_investment_in_million <- round(fundingtype$avg_investment/1000000,2)

#Which is the best funding type for spark funds?
#Venture (Avg~11.75 million USD) would be most suited for spark funds 

#Checkpoints - Part 2

#Table 3.1 #Checkpoint 3 (Country Analysis) ----------------------

# checking the summary of country_code variable
summary(factor(master_frame$country_code))

# Some of the country codes are blanks:
length(which(master_frame$country_code == ""))
length(which(master_frame$country_code == ""))/nrow(master_frame)
# 7.5%

top9<- master_frame %>%
       filter(funding_round_type == "venture" & country_code!="") %>%
       group_by(country_code) %>%
       summarise(total_investment = sum(raised_amount_usd, na.rm = T)) %>%
       arrange(desc(total_investment)) %>%
       top_n(9, total_investment)

#Adding Country-Name for each of the 9 country_code
top9$country_Name <- countrycode(top9$country_code, "iso3c", "country.name")

#"USA"(United States of America) | "GBR" (United Kingdom) | "IND"(India) 
#are the top three English Speaking 

#Checkpoint 4 (Sector Analysis 1) ------------------------------

#creating primary sector for companies by splitting category_list
# Extracting word before "|" for each category as the primary sector
master_frame<- separate(master_frame,"category_list", into="Primary_Sector", sep="\\|",extra = 'drop')

#Reading the data of mapping file
mapping <- read.csv("Course 1/Investmentcasestudy/mapping.csv",stringsAsFactors = F)

# You will observe that some of "category_list" strings 
# are spelled wrong such as "0notechnology" [index- 472]should be "nanotechnology" 
mapping$category_list[472]
#or "0tural Language Processing"[index- 473] should be 
# "natural language processing". 
mapping$category_list[473]

# But incorrect spelling follows a consistent pattern. You can inspect that the "na" is replaced with "0" in both 
# the above examples and also where the na comes along. 

# Count the number of 0s in the category_list
sum(str_count(mapping$category_list, "0"))
#54

# Hence we use str_replace_all to replace '0' by 'na'
mapping$category_list <- str_replace_all(mapping$category_list,"[0]","na")
sum(str_count(mapping$category_list, "0"))
#0

# However, this may also end up replacing 0 in strings with na, 
# even when we don't want it to.
# "enterprise 2.0" could change to "enterprise 2.na". 
# Let's treat this as well:

mapping$category_list[which(mapping$category_list=="Enterprise 2.na")] <- "Enterprise 2.0"

#changing Mapping file from wide data to long data format
mapping<- gather(mapping, main_sector, value , 2:10)
mapping <- mapping[!(mapping$value == 0),]
mapping <- mapping[,-3]

#ensuring same case in the reference variables before merging
mapping$category_list <- tolower(mapping$category_list)
master_frame$Primary_Sector <- tolower(master_frame$Primary_Sector)

#creating a new column "check" in master_frame which stores TRUE, if the sector matches with mapping sector list. 
master_frame$check = master_frame$Primary_Sector %in% mapping$category_list
sum(!master_frame$check)
# 79
# So, 79 observations do not match with the sectors in mapping file. 
# which means 79/114949 i.e 0.068% of rows do not have sectors. 

#merging mapping dataframe with master_frame
master_frame1<- merge(master_frame, mapping, by.x = 'Primary_Sector', by.y = 'category_list', all.x = T)

#Checkpoint 5 (Sector Analysis 2) ------------------------------

#country_code == "USA" | "GBR" | "IND"
#filtering the data for the top 3 countries, in the 5-15 million range for venture type 
D1 <- master_frame1 %>%
      filter(country_code == "USA" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) 

D2 <- master_frame1 %>%
      filter(country_code == "GBR" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) 
  
D3 <- master_frame1 %>%
      filter(country_code == "IND" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) 
  
#creating a summary table for  investments in the three countries
#finding the top sectors, their count of investments and total amount invested
D1_1 <- D1 %>%
        group_by(main_sector) %>%
        summarise(number_investment = n(),
        total_investment = sum(raised_amount_usd, na.rm = T))

D2_1 <- D2 %>%
        group_by(main_sector) %>%
        summarise(number_investment = n(),
        total_investment = sum(raised_amount_usd, na.rm = T))

D3_1 <- D3 %>%
        group_by(main_sector) %>%
        summarise(number_investment = n(),
        total_investment = sum(raised_amount_usd, na.rm = T))

#merging the summary table with the filtered
D1<-merge(D1, D1_1, by='main_sector')
D2<-merge(D2, D2_1, by='main_sector')
D3<-merge(D3, D3_1, by='main_sector')

#figuring the top 3 sectors and their count of investment in the three countries
D1_2<- D1_1 %>%
       filter(!is.na(main_sector)  & main_sector != "" & main_sector != "Blanks") %>%
       top_n(3, number_investment) %>%
       arrange(desc(number_investment))
#for D1, top 2 sectors are Others & Social..Finance..Analytics..Advertising
#figuring out the top company in USA in top two sectors
D1 %>%
  filter(main_sector == "Others")%>%
  arrange(desc(raised_amount_usd)) %>%
  slice(1) %>%
  select(name)

D1 %>%
  filter(main_sector == "Social..Finance..Analytics..Advertising")%>%
  arrange(desc(raised_amount_usd)) %>%
  slice(1) %>%
  select(name)

#finding the top 3 sectors in UK
D2_2<- D2_1 %>%
  filter(!is.na(main_sector)  & main_sector != "" & main_sector != "Blanks") %>%
  top_n(3, number_investment) %>%
  arrange(desc(number_investment))

#for D2, top 2 sectors are Others & Social..Finance..Analytics..Advertising
#figuring out the top company in GBR in top two sectors
D2 %>%
  filter(main_sector == "Others")%>%
  arrange(desc(raised_amount_usd)) %>%
  slice(1) %>%
  select(name)

D2 %>%
  filter(main_sector == "Social..Finance..Analytics..Advertising")%>%
  arrange(desc(raised_amount_usd)) %>%
  slice(1) %>%
  select(name)

#finding the top 2 sectors in IND
D3_2<- D3_1 %>%
  filter(!is.na(main_sector)  & main_sector != "" & main_sector != "Blanks") %>%
  top_n(3, number_investment) %>%
  arrange(desc(number_investment))

#for D3, top 2 sectors are Others &  Social..Finance..Analytics..Advertising
#figuring out the top company in IND in top two sectors
D3 %>%
  filter(main_sector == "Others")%>%
  arrange(desc(raised_amount_usd)) %>%
  slice(1) %>%
  select(name)

D3 %>%
  filter(main_sector == "Social..Finance..Analytics..Advertising")%>%
  arrange(desc(raised_amount_usd)) %>%
  slice(1) %>%
  select(name)
