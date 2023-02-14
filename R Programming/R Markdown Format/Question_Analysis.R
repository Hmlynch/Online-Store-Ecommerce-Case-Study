## Import Library
library(tidyverse)
library(ggplot2)
library(dplyr)
library(scales)

## Import cleaned data
library(readr)
df <- read_csv("Desktop/Online store Case Study/R Programming/cleaned data/cleaned_data.csv")
View(df)

### Begin Answering Questions

## Question 1: Does the date influence spending amount (Year/Month)?
# Per Year
yearly_differences_amount_spent = df %>% 
  mutate(Year = format(Transaction_date, "%Y")) %>% 
  group_by(Year) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent))
head(yearly_differences_amount_spent)
# Per Month
monthly_differences_amount_spent = df %>% 
  mutate(Month = format(Transaction_date, "%m")) %>% 
  group_by(Month) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent))
View(monthly_differences_amount_spent)
# Per both year and month
date_differences_amount_spent = df %>% 
  mutate(Month = format(Transaction_date, "%m"), Year = format(Transaction_date, "%Y")) %>% 
  group_by(Month,Year) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent))
View(date_differences_amount_spent)

## Question 2: Do certain states spend more than others?
# Total amount spent per state
state_differences_amount_spent = df %>% 
  group_by(State_names) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent),
            Avg_Amount_Spent = mean(Amount_spent))
# Top performing states to the top of dataset
state_differences_amount_spent = state_differences_amount_spent[rev(order(state_differences_amount_spent$Total_Amount_Spent)),]
# Rounding to the second decimal
state_differences_amount_spent$Total_Amount_Spent = round(state_differences_amount_spent$Total_Amount_Spent,2)
state_differences_amount_spent$Avg_Amount_Spent = round(state_differences_amount_spent$Avg_Amount_Spent,2)
# View new dataset
View(state_differences_amount_spent)
head(state_differences_amount_spent)

## Question 3: Does marital status dictate membership segments?
# Total and average amount spent per marital status per membership
marital_status_dictate_membership = df %>% 
  group_by(Marital_status,Segment) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent),Avg_Amount_Spent = mean(Amount_spent))
marital_status_dictate_membership = marital_status_dictate_membership[order(marital_status_dictate_membership$Segment),]
head(marital_status_dictate_membership)
View(marital_status_dictate_membership)

## Question 4: What is the percentage breakdown between employee status?
employee_status_percent = df %>% 
  group_by(Employees_status) %>% 
  count(Employees_status)
# Turning total count to percent of people (using scales package)
employee_status_percent$n = employee_status_percent$n/1743
employee_status_percent$Percent = percent(employee_status_percent$Percent)
# Changing the name of column
names(employee_status_percent)[names(employee_status_percent) == 'n'] = "Percent"

head(employee_status_percent)
View(employee_status_percent)

## Question 5: What age group spends more than others, how does the payment method influence age group spending?
# Making Age_Group Column and new dataframe for the question
age_group_df = df %>% 
  mutate(
    Age_Group = dplyr::case_when(
      Age < 25 ~ "15-24",
      Age >= 25 & Age < 40 ~ "25-39",
      Age >= 40 & Age < 55 ~ "40-54",
      Age >= 55 ~ "55+"
      )
    )
View(age_group_df)
age_group_differences_amount_spent = age_group_df %>% 
  group_by(Age_Group) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent), Avg_Amount_Spent = mean(Amount_spent))
head(age_group_differences_amount_spent)
# Get count of dataset
example = age_group_df %>% 
  count(Age_Group)
names(example)[names(example) == 'n'] = "Total People"
head(example)
# Combine both tables
age_group_differences_amount_spent$Total_People = example$`Total People`
head(age_group_differences_amount_spent)

## Question 6: Are referrals worth investing into?
referal_amount_spent = df %>% 
  group_by(Referal) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent), Avg_Amount_Spent = mean(Amount_spent))
head(referal_amount_spent)

## Question 7: Should other payment methods be targeted and influenced?
payment_method_targeting = df %>% 
  group_by(Payment_method) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent), Avg_Amount_Spent = mean(Amount_spent))
head(payment_method_targeting)

## Question 8: How much of a difference are the different segments making?
# Creating table with total/avg amount spent per segment
segment_influence = df %>% 
  group_by(Segment) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent), Avg_Amount_Spent = mean(Amount_spent))
head(segment_influence)
# Adding total people column
segment_count = df %>% 
  count(Segment)
head(segment_count)
segment_influence$Total_People = segment_count$n
# Adding percent column
segment_count$n = segment_count$n / 1743
segment_influence$Percent = segment_count$n
segment_influence$Percent = percent(segment_influence$Percent)
head(segment_influence)

## Question 9: In the varying states, which age group should be targeted, what percent do they make up in the states?
# Creating base table
state_targeting = age_group_df %>% 
  group_by(State_names, Age_Group) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent), Avg_Amount_Spent = mean(Amount_spent))
View(state_targeting)
# Adding total people using count
state_agegroup_count = age_group_df %>% 
  group_by(State_names,Age_Group) %>% 
  count(Age_Group)
state_agegroup_count
state_targeting$Total_People = state_agegroup_count$n
# Changing the order of the columns
state_targeting = state_targeting[,c(1,2,5,3,4)]
View(state_targeting)

## Question 10: Should we influence a gender for a specific segment?
# Creating table for total/avg amount spent
gender_segment_influence = df %>% 
  group_by(Gender,Segment) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent), Avg_Amount_Spent = mean(Amount_spent))
gender_segment_influence
# Re-ordered the values based on segment
gender_segment_influence = gender_segment_influence[order(gender_segment_influence$Segment),]
gender_segment_influence
# Created total people from different table using count
gender_segment_count = df %>% 
  group_by(Gender,Segment) %>% 
  count(Segment)
gender_segment_count
gender_segment_count = gender_segment_count[order(gender_segment_count$Segment),]
gender_segment_count
# After re-ordering the count, I merged both of them together
gender_segment_influence$Total_People = gender_segment_count$n
gender_segment_influence = gender_segment_influence[,c(1,2,5,3,4)]
gender_segment_influence

## Question 11: What age group is worth referring to the online environment?
# Creating table for age_groups referred total/avg spending
age_group_online_experience = age_group_df %>% 
  group_by(Age_Group,Referal) %>% 
  summarise(Total_Amount_Spent = sum(Amount_spent), Avg_Amount_Spent = mean(Amount_spent))
age_group_online_experience
# Separate table for total people
age_group_online_count = age_group_df %>% 
  group_by(Age_Group,Referal) %>% 
  count(Referal)
age_group_online_count
# Add column from count to main table
age_group_online_experience$Total_People = age_group_online_count$n
age_group_online_experience
# Re-ordered table 
age_group_online_experience = age_group_online_experience[,c(1,2,5,3,4)]
age_group_online_experience