# Data Wrangling Example 2\

#1.) Import the .csv file into R as a data frame replacing all blanks with NA.
library(readr)
titanic3 <- read.csv("~/R Files/Examples/Data Wrangling/titanic_original.csv", na.strings=c("", "NA")) 
str(titanic3) # This verifies the items was uploaded as a data frame.

#2.) Replace missing values in the embarkation column.
#a.) View all of the unique combinations for the embarked column.
library(dplyr)
titanic3 %>% select(embarked) %>% distinct()

#b.) Create a separate data frame that replaces NA spaces in the embarked column with S.
titanic3_NA_replaced <-
titanic3 %>% 
  filter(is.na(embarked) == TRUE) %>%
  mutate(embarked = "S")

nrow(titanic3_NA_replaced) #3 rows

#c.) Create a separate data frame that captures non-NA spaces in the embarked column.
titanic3_embarked_non_NA <-
titanic3 %>% 
  filter(is.na(embarked) == FALSE)

nrow(titanic3_embarked_non_NA) #1307 rows

#d.) Union the two data sets together.
titanic3 <- union_all(titanic3_embarked_non_NA, titanic3_NA_replaced)

#e.) Verify all NA in the embarked column has been replaced with S and that the data frame still has the original 1310 rows.
titanic3 %>% select(embarked) %>% distinct()
nrow(titanic3) #1310 rows

#3.) Replace missing values in the age column.
#a.) Calculate the mean of the age column and use that value to populate the missing values for a separate data frame.
replace_age <- mean(titanic3$age, na.rm = TRUE) #29.88113
titanic3_NA_replaced <- titanic3 %>% filter(is.na(age) == TRUE) %>% mutate(age = replace_age) 
nrow(titanic3_NA_replaced) #264 rows

#b.) Create a separate data frame that captures non-NA spaces.
titanic3_age_non_NA <-
  titanic3 %>% 
  filter(is.na(age) == FALSE)
nrow(titanic3_age_non_NA) #1046 rows

#c.) Union the two data sets together.
titanic3 <- union_all(titanic3_age_non_NA, titanic3_NA_replaced)

#d.) Verify all NA in the embarked column is still replaced with S
# , verify all NA in the age column has been replaced with the mean i.e. no NA should appear.
# , and that the data frame still has the original 1310 rows.
titanic3 %>% select(embarked) %>% distinct()
titanic3 %>% select(age) %>% distinct() %>% filter(is.na(age) == TRUE)
nrow(titanic3) #1310 rows

#e.) Other options would have been the median or the mode.

# The median is a better option than the mean because it is not just an arithmetic average. 
# The median is the middle most value in the data sets instead of just the average. It is more likely to represent the value
# ...at the top of a bell curve with the lower values to the left and the higher values to the right.
library(readr)
titanic_orig <- read.csv("~/R Files/Examples/Data Wrangling/titanic3.csv", na.strings=c("", "NA")) 
median(titanic_orig$age, na.rm = TRUE) #28 from the orignal table

# The best option is likely using mode because netither the mean or the median takes into account the number of occurences.
# We don't really wish to find the average or the middle point; we really want to default the age to the most likely age
#...based on the number of occurences.
# Can create a mode user-definedfunction.
uniqv <- titanic_orig %>% group_by(age) %>%  summarize(mode = n()) %>%  arrange (desc(mode)) %>% filter(age != "NA") %>% top_n(1)
uniqv %>% select(age) #24

#4.) Replace missing values in the boat column with NA.
# This was accomplised at the time the original file was imported.
titanic3 %>% select(boat) %>% distinct() 

#5.) A missing cabin value might mean that the passagenger did not survive. Create a new column "has_cabin_number" which 
#...has 1 if there is a cabin number, and 0 otherwise.
titanic3 <- mutate(titanic3, has_cabin_number = if_else(is.na(cabin) == TRUE, 0, 1))
titanic3 %>% select(cabin, has_cabin_number) %>% distinct() 

#6.) Write out results to .csv file.
library(readr)
write.csv(file="C:/Users/hender324/Documents/R Files/Examples/Data Wrangling/titanic_clean.csv", x=titanic3)