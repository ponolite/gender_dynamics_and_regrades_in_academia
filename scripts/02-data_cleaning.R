#### Preamble ####
# Purpose: Cleans the raw plane data recorded by two observers..... [...UPDATE THIS...]
# Author: Catherine Punnoose, Quang Mai, Faiza Imam 
# Date: 13 Feb 2024 
# Contact: catherine.punnoose@mail.utoronto.ca
# License: MIT
# Pre-requisites: [...UPDATE THIS...]
# Any other information needed? [...UPDATE THIS...]

#### Workspace setup ####
library(tidyverse)

### CLEANING DATA ###
### Cleaning Instructor	Survey	Data (instsurvey) ###
inst_data <- read.csv(here::here("inputs/data/instsurvey.csv"))
inst_data_cleaned <- 
  inst_data |>
  janitor::clean_names() |>
  select(pctrequests_fn) |>
  tidyr::drop_na()


### Cleaning Stdsurvey ###
std_data <- read.csv(here::here("inputs/data/stdsurvey.csv"))

std_data_cleaned <- 
  std_data |>
  janitor::clean_names() |>
  select(std_female, std_standing, std_male, consider_regrade, everasked, num_class, num_asked) |>
  tidyr::drop_na()
  
#### Save Clean Stdsurvey data ####
write_csv(std_data_cleaned, "outputs/data/std_data_cleaned.csv")

### Cleaning Experiment ###
#exp_data <- read.csv(here::here("inputs/data/experiment.csv"))



