# Data Wrangling Example 1

#1.) Import the .csv file into R
library(readr)
company_brands <- read_csv("refine_original_unchanged.csv")

#2.) Ensure all column names are lowercase
names(company_brands) <- tolower(names(company_brands))

#3.) Standarize the company names.
#a.) View all of the unique combinations. 
library(dplyr)
company_brands %>% select(company) %>% distinct()

#b.) Set all values in the company column to lowercase.
company_brands$company <- tolower(company_brands$company)

#c.) Remove all spaces in the company column.
company_brands$company <- gsub("ak zo", "akzo", company_brands$company) # ak zo

#d.) Change all zeros to Os in the company column.
company_brands$company <- gsub("0", "o", company_brands$company) #akz0

#e.) Change all fi to phi in the company column.
company_brands$company <- gsub("fi", "phi", company_brands$company) #fillips

#f.) Change all unilver to unilever  in the company column.
company_brands$company <- gsub("unilver", "unilever", company_brands$company) #unilver

#g.) Identify all of the phillips
companyname <- company_brands$company
phillips_hits <- grepl(pattern = "ps", x = companyname)

#h.) Bind vectors as columns to the existing dataframe.
library(tidyr)
company_brands2 <- cbind(company_brands, phillips_hits)

#i.) Transform all company names where phillips_hits is TRUE.
company_brands3 <- company_brands2 %>% 
  filter(phillips_hits == "TRUE") %>%
  mutate(company = "phillips")

#j.) Select all company names where phillips_hits is FALSE.
company_brands1 <- company_brands2 %>% 
  filter(phillips_hits == "FALSE") 

#k.) Union company_brands3 to company_brands1
company_brands <- rbind(company_brands3, company_brands1)

#3.) separate out product code and product number
library(tidyr)
company_brands <- separate(company_brands, `product code / number`, c("product_code", "product_number"), sep = "-")

#4.) Add product name - loop through values.
#a.) Create a product update table.
company_brands_product_nm <- data.frame(product_code=c('p', 'v', 'x', 'q'),
                 product_name=c('SmartPhone','TV','Laptop', 'Tablet'), stringsAsFactors=F)

#b.) Use a left join to update the product_name column.
company_brands <- left_join(company_brands, company_brands_product_nm, by = "product_code")

#4.) Concatenate address, city, and country into the full_address column.

company_brands <- unite(company_brands, "full_address", address, city, country, sep = " ")

#5.) Create binary dummy variables 
# create dummy variables for company

# company_phillips

library(dplyr)
company_brands <- mutate(company_brands, company_philips = if_else(company == "phillips", 1, 0))

# company_akzo

library(dplyr)
company_brands <- mutate(company_brands, company_akzo = if_else(company == "akzo", 1, 0))

# company_van_houten

library(dplyr)
company_brands <- mutate(company_brands, company_van_houten = if_else(company == "van houten", 1, 0))

# company_unilever

library(dplyr)
company_brands <- mutate(company_brands, company_unilever = if_else(company == "unilever", 1, 0))

# create dummy variables for product_category
# product_smartphone

library(dplyr)
company_brands <- mutate(company_brands, product_smartphone = if_else(product_name == "SmartPhone", 1, 0))

# product_tv

library(dplyr)
company_brands <- mutate(company_brands, product_tv = if_else(product_name == "TV", 1, 0))

# product_laptop

library(dplyr)
company_brands <- mutate(company_brands, product_laptop = if_else(product_name == "Laptop", 1, 0))

# product_tablet

library(dplyr)
company_brands <- mutate(company_brands, product_tablet = if_else(product_name == "Tablet", 1, 0))

#6.) Select all columns except for a few remaining data manipulation columns.
company_brands <- select(company_brands, - phillips_hits)

#7.) Write out results to .csv file.
library(readr)
write.csv(file="C:/Users/hender324/Documents/R Files/Examples/Data Wrangling/refine_clean.csv", x=company_brands)

