#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from https://www.voterstudygroup.org/downloads?key=dd05a560-c382-42f1-a3e4-7e5318f9781a
# (UCLA + Democracy Fund)
# Author: Eric Yuan
# Data: 2 November 2020
# Contact: eric.yuan@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from Democracy Fund and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!

setwd("~/Desktop/2019-canadian-federal-election-analysis/data-cleaning")

#### Workspace setup ###
library(haven)
library(plyr)
library(tidyverse)

# Download dataset
cesR::get_ces("ces2019_web")

# Add the labels
raw_data <- labelled::to_factor(ces2019_web)

# Just keep some variables
reduced_data <- raw_data %>% 
  filter(cps19_citizenship == "Canadian citizen") %>%
  select(cps19_gender,
         cps19_province,
         cps19_yob, 
         cps19_education, 
         cps19_votechoice, 
         cps19_income_cat) %>%
  na.omit(ces2019_web)

# Add age column and remove yob
reduced_data <- reduced_data %>% 
  dplyr::mutate(age = 100 - as.integer(cps19_yob)) %>%
  select(-cps19_yob)

# Rename the columns
reduced_data <- reduced_data %>% 
  dplyr::rename(gender = cps19_gender,
                education = cps19_education,
                vote2019 = cps19_votechoice,
                income = cps19_income_cat,
                province = cps19_province)

# Remove Don't know/ Prefer not to answer"
reduced_data <- reduced_data %>%
  filter(vote2019 != "Don't know/ Prefer not to answer")

# Remove "Other" in gender to match census
reduced_data <- reduced_data %>%
  filter(gender != "Other (e.g. Trans, non-binary, two-spirit, gender-queer)")

# Remove all "Don't know/ Prefer not to answer" entries
reduced_data <- reduced_data %>%
  filter(education != "Don't know/ Prefer not to answer",
         vote2019 != "Don't know/ Prefer not to answer",
         income != "Don't know/ Prefer not to answer")

# Map education to Census columns
education_mapping = c("Master's degree" = "Master's degree",
                      "Some university" = "Bachelor's degree",
                      "Completed secondary/ high school" = "Secondary (high) school diploma or equivalency certificate",
                      "Some secondary/ high school" = "Secondary (high) school diploma or equivalency certificate",
                      "Bachelor's degree" = "Bachelor's degree",
                      "Completed technical, community college, CEGEP, College Classique" = "College, CEGEP or other non-university certificate or diploma",
                      "Some technical, community college, CEGEP, College Classique" = "College, CEGEP or other non-university certificate or diploma",
                      "Professional degree or doctorate" = "Earned doctorate",
                      "Some elementary school" = "No certificate, diploma or degree",
                      "Completed elementary school" = "No certificate, diploma or degree",
                      "No schooling" = "No certificate, diploma or degree")

reduced_data <- reduced_data %>%
  mutate(education=revalue(education, education_mapping))

# Map age to Census columns
reduced_data <- reduced_data %>%
  mutate(age = ifelse(age <= 19, "18 to 19 years",
               ifelse(age <= 24, "20 to 24 years",
               ifelse(age <= 29, "25 to 29 years",
               ifelse(age <= 34, "30 to 34 years",
               ifelse(age <= 39, "35 to 39 years",
               ifelse(age <= 44, "40 to 44 years",
               ifelse(age <= 49, "45 to 49 years",
               ifelse(age <= 54, "50 to 54 years",
               ifelse(age <= 59, "55 to 59 years",
               ifelse(age <= 64, "60 to 64 years",
               ifelse(age <= 69, "65 to 69 years",
               ifelse(age <= 74, "70 to 74 years",
               ifelse(age <= 79, "75 to 79 years",
               ifelse(age <= 84, "80 to 84 years", 
                   "85 years and over")))))))))))))))



# Map province to Census columns
province_mapping = c("Yukon" = "Northern Canada",
                     "Northwest Territories" = "Northern Canada",
                     "Nunavut" = "Northern Canada")
reduced_data <- reduced_data %>%
  mutate(province=revalue(province, province_mapping))

# Change ndp to NDP
vote2019_mapping = c("ndp" = "NDP")
reduced_data <- reduced_data %>%
  mutate(vote2019=revalue(vote2019, vote2019_mapping))

# Create vote_trudeau column
reduced_data <- reduced_data %>%
  mutate(vote_trudeau=ifelse(vote2019 == "Liberal Party", 1, 0))

