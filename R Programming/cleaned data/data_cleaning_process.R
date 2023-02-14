# Import Packages
library(tidyverse)
library(ggplot2)
library(dplyr)
library(readr)
# Import unclean data
df <- read_csv("Desktop/Online store Case Study/R Programming/uncleaned data/online_store_customer_data copy.csv")
View(df)

## Step 1 Data Cleaning Process: Remove "NA" rows
new_df = na.omit(df)
View(new_df)

## Step 2 Data Cleaning Process: Remove all 2021 rows
# First change string date to date format
new_df = mutate(new_df, Transaction_date = as.Date(Transaction_date, "%m/%d/%Y"))
class(new_df$Transaction_date)
head(new_df)
# Second remove the rows with 2021
new_df = new_df %>% 
  filter(Transaction_date < "2021-01-01")
View(new_df)

## Step 3 Data Cleaning Process: Rename values in Referal column
new_df$Referal[new_df$Referal == 1] = 'Reffered'
new_df$Referal[new_df$Referal == 0] = 'Not Referred'
head(new_df)
View(new_df)

## Step 4 Data Cleaning Process: Save cleaned data file
save(new_df, file = "cleaned_data.csv")
write.csv(new_df, file = "cleaned_data.csv")



