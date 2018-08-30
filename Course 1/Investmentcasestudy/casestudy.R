pkgs <- c("dplyr", "tidyr", "stringr", "ISLR")
lapply(pkgs, require, character.only = T)

#checkpoint 1
#read the companies dataset
#fill = T is needed because data does not exist for all companies
companies<- read.delim("Course 1/Investmentcasestudy/companies.txt",sep  = "\t", stringsAsFactors = FALSE, header=TRUE)
str(companies)

#read the funding rounds details
rounds2<- read.csv("Course 1/Investmentcasestudy/rounds2.csv")
str(rounds2)
#changing company_permalink to a character type to match companies dataset
rounds2$company_permalink <- as.character(rounds2$company_permalink)

#unique companies present in rounds2
length(unique(rounds2$company_permalink))
#cross checking by looking for duplicates
sum(duplicated(rounds2$company_permalink))

#unique companies present in companies
length(unique(companies$permalink))
sum(duplicated(companies$permalink))

#Are there any companies in the rounds2 file which are not present in companies?
#changing both columns to lowercase
rounds2$company_permalink<- tolower(rounds2$company_permalink)
companies$permalink<- tolower(companies$permalink)
#using setdiff from dplyr packageto count intersection b/w both columns
#have added unique() because a company may have gone through multiple funding rounds
length(setdiff(unique(rounds2$company_permalink), unique(companies$permalink)))

#merging these 2 datasets together
master_frame<- rounds2 %>%
  left_join(companies, by = c("company_permalink" = "permalink"))

#checkpoint 2

#funding types available in the dataset can be found by
summary(master_frame$funding_round_type)
# we are interested in four kinds of funding: venture, angel, seed, private equity

fundingtype<- master_frame %>%
  filter(funding_round_type == "venture" | funding_round_type == "angel" |funding_round_type == "seed" |funding_round_type == "private_equity") %>%
  group_by(funding_round_type) %>%
  summarise(avg_investment = mean(raised_amount_usd, na.rm = T))

#which is the best funding type for spark funds?
#venture funding would be most suited for spark funds 

#country analysis
top9<- master_frame %>%
  filter(funding_round_type == "venture") %>%
  group_by(country_code) %>%
  summarise(total_investment = sum(raised_amount_usd, na.rm = T)) %>%
  arrange(desc(total_investment)) %>%
  top_n(9, total_investment)

#country_code == "USA" | "GBR" | "IND" are the top three English Speaking Countries present in the dataset

#checkpoint 4: Sector Analysis

#creating primary sector for companies by splitting category_list
master_frame<- separate(master_frame,"category_list", into="Primary_Sector", sep="\\|",extra = 'drop')

#reading in the file containing sector classification of companies
mapping <- read.csv("Course 1/Investmentcasestudy/mapping.csv")
#replacing 0s in category_list with 'na's
mapping$category_list<- str_replace_all(mapping$category_list, "0", "na")
#changing from wide data to long data format
mapping<- gather(mapping, key = main_sector, value = value, 2:10)
mapping <- mapping[!(mapping$value == 0),]
mapping <- mapping[,-3]

#ensuring same case in the reference variables
mapping$category_list <- tolower(mapping$category_list)
master_frame$Primary_Sector <- tolower(master_frame$Primary_Sector)

#merging mapping dataframe with master_frame
master_frame1<- merge(master_frame, mapping, by.x = 'Primary_Sector', by.y = 'category_list', all.x = T)

#country_code == "USA" | "GBR" | "IND"
#filtering for investment in the range 5-15 million, inclusive
D1 <- master_frame1 %>%
  filter(country_code == "USA" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) %>%
  group_by(main_sector) %>%
  mutate(number_investment = n(),
         total_investment = sum(raised_amount_usd, na.rm = T))

D2 <- master_frame1 %>%
  filter(country_code == "GBR" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) %>%
  group_by(main_sector) %>%
  mutate(number_investment = n(),
         total_investment = sum(raised_amount_usd, na.rm = T))

D3 <- master_frame1 %>%
  filter(country_code == "IND" & funding_round_type == "venture" & raised_amount_usd >= 5000000 & raised_amount_usd <= 15000000) %>%
  group_by(main_sector) %>%
  mutate(number_investment = n(),
         total_investment = sum(raised_amount_usd, na.rm = T))


