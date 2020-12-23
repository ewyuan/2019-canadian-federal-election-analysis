#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from IPUMS USA https://doi.org/10.18128/D010.V10.0
# Author: Eric Yuan
# Data: 2 November 2020
# Contact: eric.yuan@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!

setwd("~/Desktop/2019-canadian-federal-election-analysis/data-cleaning")

#### Workspace setup ####
library(haven)
library(plyr)
library(tidyverse)

# Load data
raw_data_census <- read_csv("AAkBTY3j.csv")

# Rename columns
reduced_data_census <- raw_data_census %>%
  select(-CASEID) %>%
  dplyr::rename(age = agegrp,
                gender = sex,
                education = hdgree,
                income = totinc,
                province = pr)

# Convert age to categories
reduced_data_census <- reduced_data_census %>%filter(age >= 7) %>%
  mutate(age = ifelse(age == 7, "18 to 19 years",
               ifelse(age == 8, "20 to 24 years",
               ifelse(age == 9, "25 to 29 years",
               ifelse(age == 10, "30 to 34 years",
               ifelse(age == 11, "35 to 39 years",
               ifelse(age == 12, "40 to 44 years",
               ifelse(age == 13, "45 to 49 years",
               ifelse(age == 14, "50 to 54 years",
               ifelse(age == 15, "55 to 59 years",
               ifelse(age == 16, "60 to 64 years",
               ifelse(age == 17, "65 to 69 years",
               ifelse(age == 18, "70 to 74 years",
               ifelse(age == 19, "75 to 79 years",
               ifelse(age == 20, "80 to 84 years", 
                      "85 years and over")))))))))))))))

# Convert sex to categories
reduced_data_census <- reduced_data_census %>%
  mutate(gender = ifelse(gender == 1, "A woman", 
                         "A man"))

# Convert education to categories
reduced_data_census <- reduced_data_census %>%
  filter(education <= 2 | education == 5 | education == 6 | education == 7 | education == 9 | education == 12 | education == 13) %>%
  mutate(education = ifelse(education == 1, "No certificate, diploma or degree",
                     ifelse(education == 2, "Secondary (high) school diploma or equivalency certificate",
                     ifelse(education == 9, "Bachelor's degree",
                     ifelse(education == 12, "Master's degree",
                     ifelse(education == 13, "Earned doctorate", 
                            "College, CEGEP or other non-university certificate or diploma"))))))

# Convert province to categories
reduced_data_census <- reduced_data_census %>%
  mutate(province = ifelse(province == 10, "Newfoundland and Labrador",
                    ifelse(province == 11, "Prince Edward Island",
                    ifelse(province == 12, "Nova Scotia",
                    ifelse(province == 13, "New Brunswick",
                    ifelse(province == 24, "Quebec",
                    ifelse(province == 35, "Ontario",
                    ifelse(province == 46, "Manitoba",
                    ifelse(province == 47, "Saskatchewan",
                    ifelse(province == 48, "Alberta",
                    ifelse(province == 59, "British Columbia", "Northern Canada")))))))))))

# Convert income to categories
reduced_data_census <- reduced_data_census %>%
  filter(income != 88888888 | income != 99999999) %>%
  mutate(income = ifelse(income <= 0, "No income", 
                  ifelse(income <= 30000, "$1 to $30,000", 
                  ifelse(income <= 60000, "$30,001 to $60,000",
                  ifelse(income <= 90000, "$60,001 to $90,000",
                  ifelse(income <= 110000, "$90,001 to $110,000",
                  ifelse(income <= 150000, "$110,001 to $150,000",
                  ifelse(income <= 200000, "$150,001 to $200,000", "More than $200,000"))))))))

reduced_data_census_count <- reduced_data_census %>%
  dplyr::count(age, province, education, income, gender) %>%
  group_by(age, province, education, income, gender)

#reduced_data_census_count <- reduced_data_census_count %>%
#  mutate(age = as.factor(age),
#         province = as.factor(province),
#         education = as.factor(education),
#         income = as.factor(income),
#         gender = as.factor(gender))



         