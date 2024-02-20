#### Preamble ####
<<<<<<< HEAD
# Purpose: Cleans the data collected from the article ‘Ask and You Shall Receive? Gender differences in Regrades in College’
=======
# Purpose: Clean up the survey.dta from https://doi.org/10.7910/DVN/QXJDJ5. Specifically selecting columns participantcode, std_female, std_standing, std_male, consider_regrade, everasked, num_class, num_asked).
>>>>>>> 3d9be9f6bb0cc6db6eb351c33fca37c617e7c9c0
# Author: Catherine Punnoose, Quang Mai, Faiza Imam 
# Date: 13 Feb 2024 
# Contact: catherine.punnoose@mail.utoronto.ca, q.mai@mail.utoronto.ca, faiza.imam@mail.utoronto.ca
# License: MIT
# Pre-requisites: 01-download_data.R

#### Workspace setup ####
library(dplyr)
library(tidyr)
library(janitor)
library(tidyverse)

### CLEANING DATA ###
### Cleaning Instructor	Survey	Data (instsurvey) ###
# Read data
inst_data <- read.csv("inputs/data/raw data/instsurvey.csv")

inst_data_cleaned <- 
  inst_data |>
  janitor::clean_names() |>
  select(id, maleup_fn, femaleup_fn, malesame_fn, femalesame_fn, maledown_fn, femaledown_fn, maleup_mt, femaleup_mt, malesame_mt, femalesame_mt, maledown_mt, femaledown_mt, inst_female) |>
  mutate(inst_gender = case_match(inst_female, 
                                0 ~ "Male",
                                1 ~ "Female")) |>
  select(-inst_female) |>
  tidyr::drop_na()

#### Save Clean Instsurvey data ####
write_csv(inst_data_cleaned, "inputs/data/cleaned data/inst_data_cleaned.csv")

### Cleaning Stdsurvey ###
std_data <- read.csv("inputs/data/raw data/stdsurvey.csv")

std_data_cleaned <- 
  std_data |>
  janitor::clean_names() |>
  select(participantcode, std_female, std_standing, std_male, consider_regrade, everasked, num_class, num_asked)

#### Save Clean Stdsurvey data ####
write_csv(std_data_cleaned, "inputs/data/cleaned data/std_data_cleaned.csv")
