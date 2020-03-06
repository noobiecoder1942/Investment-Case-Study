# Loading companies dataset using "read.delim" function (default delimiter is TAB) as this is a text file with TAB as a delimiter.
companies <- read.delim("./Investment Case Study/companies.txt", stringsAsFactors = FALSE)

# Loading rounds2 dataset using "read.csv" function
rounds2 <- read.csv("./Investment Case Study/rounds2.csv", stringsAsFactors = FALSE)

##########################################################################################################
#Filling the Sheet Table-1.1 (Checkpoint 1: Data Cleaning 1)

#1 Since R is Case Sensitive converting to lowercase so that the unique companies name can be identified
rounds2Lower <- tolower(rounds2[,1])
uniqueRounds2 <- unique(rounds2Lower)
length(uniqueRounds2)

#2 Since R is Case Sensitive converting to lowercase so that the unique companies name can be identified
companiesLower <- tolower(companies[,1])
uniqueCompanies <- unique(companiesLower)
length(uniqueCompanies)

#3 Using R's inbuilt function "all" to see if all the companies in rounds2 are present in companies or not
all(uniqueRounds2 == uniqueCompanies)

#4
# First updating the company names in both dataset to lowercase
companies[,1] <- tolower(companies[,1])
rounds2[,1] <- tolower(rounds2[,1])

# Now merging the columns on the basis of "permalink" in "companies" and "company_permalink" in "rounds2"
master_frame <- merge(companies, rounds2, by.x = "permalink", by.y = "company_permalink")


##########################################################################################################
#Filling the Sheet Table-2.1 (Checkpoint 2: Funding Type Analysis)

#1,2,3,4
# Since the "raised_amount_usd" has some missing values in the data set. Around 13770 NA's are there in all the 4 funding round type.
# We can ignore these values for now. And when creating graphs we can substitute the values with mean. Altough this will increase the total sum of the funding amount.
# Replacing these with 0's doesn't seems to be right as some of the datasets do have explicitly 0 as funding amount.
# NA's also refer to the fact that funding never happenned or it got cancelled after some initial discussions only.
# To load "group_by" using "dplyr" package. Using summarise and group_by here. We can also use aggregate function if we want.

library(dplyr)
groupByFundingRound <- group_by(master_frame, funding_round_type)
arrange(summarise(groupByFundingRound, mean = mean(raised_amount_usd, na.rm = T)), desc(mean))

#5 Answer is in the comments below.
# Of all the four investment types (venture, angel, seed, and private equity), venture has average of "USD 11748949" (~ 11.7 million) which falls in between USD 5 to 15 million.
# Other funding types have average funding amount raised as "angel ~ 0.95 million", "seed ~ 0.72 million" and "private_equity ~ 73.3 million".
# So if Spark funds has funding constraints of USD 5-15 million, they should go with "venture" funding type.


###########################################################################################################
#Filling the Sheet Table-3.1 (Checkpoint 3: Country Analysis)

#1 Grouping by country code to get the countries grouped by country and then sum over the fund amount raised to find the top countries.
country_with_venture_funding <- subset(master_frame, master_frame$funding_round_type == "venture")
groupByCountryCode <- group_by(country_with_venture_funding, country_code)
countriesWithHighestFunding <- arrange(summarise(groupByCountryCode, sum = sum(raised_amount_usd, na.rm = T)), desc(sum))

# As we can see "countriesWithHighestFunding" contains an entry with no country code and hence this is not a valid entry.
# The no. of rows with no country code is around 8678 entries which is around 7% of whole dataset and hence we can ignore these datasets.
# Also No one with any country information will want to invest in this area. So ignoring the 3rd entry entry and pushing above all the entries by one rank.
# Code to find no. of entries with no country code mentioned "length(master_frame$country_code[which(master_frame$country_code == "")])"

# Removing 3rd entry from "countriesWithHighestFunding" and restricting to only top 9 entries.
countriesWithHighestFunding <- countriesWithHighestFunding[-3,]
countriesWithHighestFunding <- countriesWithHighestFunding[0:9,]

# Finally storing the value in top9 dataframe
top9 <- countriesWithHighestFunding

#2
# Referring to the English speaking PDF, the English Speaking entries out of the countries stored in top9 dataframes are:
# According to the top9 dataframe, the top3 countries are USA, China and GBR. But China is not mentioned in PDF file as a english speaking country.
# So ignoring the "China" and instead picking "India" as the top 3rd country in the list.
# So the answer for top3 english speaking countries are (In Order): (1st) USA, (2nd) GBR, (3rd) IND


###########################################################################################################
# Checkpoint 4: Sector Analysis 1

# Extracting the primary sector from "category_list" in master_frame. As this is "|", we can use following code to do the same.
library(tidyr)
master_frame <- separate(master_frame, category_list, into = c("primary_sector"), sep = "\\|", remove = FALSE)

# Reading the "mapping.csv" file
mapping <- read.csv("./Investment Case Study/mapping.csv", stringsAsFactors = FALSE)

# Doing some cleanin on the mapping dataframe, like in some category_list names "na" word has been replaced by "0".
library(stringr)
mapping$category_list <- str_replace(mapping$category_list, pattern = "0", replacement = "na")

# Gathering all the data in different columns in mapping dataframe
mappingLong <- gather(mapping, main_sectors, value, 2:10)
mappingLong <- mappingLong[!(mappingLong$value == 0),]
mappingLong <- mappingLong[,-3]

# Now merging the master_frame and mappingLong on primar_sector and category_list respectively.
master_frame <- merge(x = master_frame, mappingLong, by.x = "primary_sector", by.y = "category_list" , all.x = TRUE)

# Since the "Blanks" category is around 2.9% only of total no. of observations. We can remove it. Code to check blank primary sectors "length(which(master_frame$primary_sector == ""))".
master_frame <- master_frame[which(!(master_frame$primary_sector == "")),]

# Now creating a dataframe and putting only "primary_sectors" and "sectors" in this.
primary_sector <- master_frame$primary_sector
main_sector <- master_frame$main_sectors
primary_main_sector <- cbind(primary_sector, main_sector)
# So the "primary_main_sector" contains the mapping of primary and main sectors. And If whole data set is needed then "master_frame" contains the same.


###########################################################################################################
# Checkpoint 5: Sector Analysis 2
# Funding round type was "Venture" and Countries(Country Code) were: USA, GBR and IND
D1_USA <- subset(master_frame, master_frame$country_code == "USA" & master_frame$funding_round_type == "venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)
D1_GBR <- subset(master_frame, master_frame$country_code == "GBR" & master_frame$funding_round_type == "venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)
D1_IND <- subset(master_frame, master_frame$country_code == "IND" & master_frame$funding_round_type == "venture" & master_frame$raised_amount_usd >= 5000000 & master_frame$raised_amount_usd <= 15000000)

# Total no. of investments and total amount invested in each main sector can be calculated from these 3 data frames as mentioned in the question
# For D1_USA
main_sector_group <- group_by(D1_USA, main_sectors)
Total_No_of_Investments_USA <- arrange(summarise(main_sector_group, Total_No_of_Investments = n()), desc(Total_No_of_Investments))
Total_Amount_Invested_USA <- arrange(summarise(main_sector_group, Total_Amount_Invested = sum(raised_amount_usd, na.rm = T)),desc(Total_Amount_Invested))
# Creaing a dataframe of above 2 values
main_sector_summary <- merge(Total_No_of_Investments_USA, Total_Amount_Invested_USA, by= "main_sectors")
# Merging the "main_sector_summary" with "D1_USA
D1_USA <- merge(D1_USA, main_sector_summary, by= "main_sectors")

# For D1_GBR
main_sector_group <- group_by(D1_GBR, main_sectors)
Total_No_of_Investments_GBR <- arrange(summarise(main_sector_group, Total_No_of_Investments = n()), desc(Total_No_of_Investments))
Total_Amount_Invested_GBR <- arrange(summarise(main_sector_group, Total_Amount_Invested = sum(raised_amount_usd, na.rm = T)),desc(Total_Amount_Invested))
# Creaing a dataframe of above 2 values
main_sector_summary <- merge(Total_No_of_Investments_GBR, Total_Amount_Invested_GBR, by= "main_sectors")
# Merging the "main_sector_summary" with "D1_GBR
D1_GBR <- merge(D1_GBR, main_sector_summary, by= "main_sectors")

# For D1_IND
main_sector_group <- group_by(D1_IND, main_sectors)
Total_No_of_Investments_IND <- arrange(summarise(main_sector_group, Total_No_of_Investments = n()), desc(Total_No_of_Investments))
Total_Amount_Invested_IND <- arrange(summarise(main_sector_group, Total_Amount_Invested = sum(raised_amount_usd, na.rm = T)),desc(Total_Amount_Invested))
# Creaing a dataframe of above 2 values
main_sector_summary <- merge(Total_No_of_Investments_IND, Total_Amount_Invested_IND, by= "main_sectors")
# Merging the "main_sector_summary" with "D1_IND
D1_IND <- merge(D1_IND, main_sector_summary, by= "main_sectors")

# Based on the above study if we see the trend, the most investments have been happening in "USA" based on the basis of highest total amount invested.
# The funding type is "Venture" as we have figured it out. And the "Others" sectors is the most prefered one among all. It has highest no. of investments and also the total amount invested.

# For Table 5.1 all the ansers are in Excel submitted for the evaluation along with PPT.

# 5.1 Q1

# For USA
sum(Total_Amount_Invested_USA$Total_Amount_Invested)
sum(Total_No_of_Investments_USA$Total_No_of_Investments)

# For GBR
sum(Total_Amount_Invested_GBR$Total_Amount_Invested)
sum(Total_No_of_Investments_GBR$Total_No_of_Investments)

# For IND
sum(Total_Amount_Invested_IND$Total_Amount_Invested)
sum(Total_No_of_Investments_IND$Total_No_of_Investments)

# 5.1 Q1-Q8, has been answered using tables

# 5.1 Q9
# For USA
subset_on_sector <- subset(D1_USA, D1_USA$main_sectors == "Others")
subset_g <- group_by(subset_on_sector, permalink)
arrange(summarise(subset_g, amount = sum(raised_amount_usd)), desc(amount))
# Answer is "/organization/virtustream"

# For GBR
subset_on_sector2 <- subset(D1_GBR, D1_GBR$main_sectors == "Others")
subset_g2 <- group_by(subset_on_sector2, permalink)
arrange(summarise(subset_g2, amount = sum(raised_amount_usd)), desc(amount))
# Answer is "/organization/electric-cloud"

# For IND
subset_on_sector <- subset(D1_IND, D1_IND$main_sectors == "Others")
subset_g <- group_by(subset_on_sector, permalink)
arrange(summarise(subset_g, amount = sum(raised_amount_usd)), desc(amount))
# Answer is: "/organization/firstcry-com"

# 5.1 Q10
# For USA
subset_on_sector <- subset(D1_USA, D1_USA$main_sectors == "Social..Finance..Analytics..Advertising")
subset_g <- group_by(subset_on_sector, permalink)
arrange(summarise(subset_g, amount = sum(raised_amount_usd)), desc(amount))
# Answer is "/organization/shotspotter"

# For GBR
subset_on_sector2 <- subset(D1_GBR, D1_GBR$main_sectors == "Social..Finance..Analytics..Advertising")
subset_g2 <- group_by(subset_on_sector2, permalink)
arrange(summarise(subset_g2, amount = sum(raised_amount_usd)), desc(amount))
# Answer is "/organization/celltick-technologies"

# For IND
subset_on_sector <- subset(D1_IND, D1_IND$main_sectors == "Social..Finance..Analytics..Advertising")
subset_g <- group_by(subset_on_sector, permalink)
arrange(summarise(subset_g, amount = sum(raised_amount_usd)), desc(amount))
# Answer is: "/organization/manthan-systems"

write.csv(master_frame, "master_frame.csv", row.names = FALSE)
