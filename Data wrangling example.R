# Upload files into R

#1.) get current working directory
getwd()  

#2.) place the file in the working directory or reset the working directory

# setwd("C:/Users/hender324/Documents/RFiles")   # set working directory

#3.) Import the data into R
company_brands = read.csv("refine_original.csv") 
company_brands

#4.) ensure all column names are lowercase

tbl_df(company_brands) # set as a local data frame

colnames(company_brands)[2] <- "product.code...number"

#5.) standarize the brand names - need help!

company_brands %>% select(company) %>% distinct()

#6.) separate out product code and product number

library(tidyr)
company_brands <- separate(company_brands, product.code...number, c("product_code", "product_number"), sep = "-")
company_brands

#7.) Add product categories need help here!

library(dplyr)
# Smartphone
company_brands1 <- company_brands %>% 
  filter(product_code == "p") %>%
  select(company, product_code, product_number, address, city, country, name) %>%
  mutate(product_code = "SmartPhone")

# TV
company_brands2 <- company_brands %>% 
  filter(product_code == "v") %>%
  select(company, product_code, product_number, address, city, country, name) %>%
  mutate(product_code = "TV")

# Laptop
company_brands3 <- company_brands %>% 
  filter(product_code == "x") %>%
  select(company, product_code, product_number, address, city, country, name) %>%
  mutate(product_code = "Laptop")

# Tablet
company_brands4 <- company_brands %>% 
  filter(product_code == "q") %>%
  select(company, product_code, product_number, address, city, country, name) %>%
  mutate(product_code = "Tablet")

company_brands1
company_brands2
company_brands3
company_brands4

company_brands <- rbind.fill(company_brands1, company_brands2, company_brands3, company_brands4)
company_brands

#8.) Concatenate address, city, and country

company_brands <- unite(company_brands, "full_address", address, city, country, sep = " ")
company_brands

#9.) Create binary dummy variables 
# create dummy variables for company

# company_phillips

library(dplyr)
company_brands <- mutate(company_brands, company_philips = if_else(company == "phillips", 1, 0))
company_brands

# company_akzo

library(dplyr)
company_brands <- mutate(company_brands, company_akzo = if_else(company == "akzo", 1, 0))
company_brands

# company_van_houten

library(dplyr)
company_brands <- mutate(company_brands, company_van_houten = if_else(company == "van houten", 1, 0))
company_brands

# company_unilever

library(dplyr)
company_brands <- mutate(company_brands, company_unilever = if_else(company == "unilver", 1, 0))
company_brands

# create dummy variables for product_category
# product_smartphone

library(dplyr)
company_brands <- mutate(company_brands, product_smartphone = if_else(product_code == "SmartPhone", 1, 0))
company_brands

# product_tv

library(dplyr)
company_brands <- mutate(company_brands, product_tv = if_else(product_code == "TV", 1, 0))
company_brands

# product_laptop

library(dplyr)
company_brands <- mutate(company_brands, product_laptop = if_else(product_code == "Laptop", 1, 0))
company_brands

# product_tablet

library(dplyr)
company_brands <- mutate(company_brands, product_tablet = if_else(product_code == "Tablet", 1, 0))
company_brands

#10.) Write to .csv file

write.csv(file="C:/Users/hender324/Documents/refine_clean.csv", x=company_brands)
