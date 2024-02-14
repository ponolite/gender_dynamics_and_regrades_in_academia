#### Test data ####
#### Preamble ####
# Purpose: Tests cleaned datasets 
# Author: Quang Mai, Catherine Punnoose and Faiza Imam
# Date: 11 February 2024
# Contact:
# License: MIT
# Pre-requisites: reload 01-download_data.R and 02-data_cleaning.R

#### Workspace setup ####
library(tidyverse)
library(dplyr)
library(janitor)


#### Read in cleaned data ####

# read cleaned data on arrests and strip searches
std_data_cleaned = read_csv(
  file = here("inputs/data/std_data_cleaned.csv"),
  show_col_types = FALSE
)

# read cleaned data on race and gender and strip searches
inst_data_cleaned = read_csv(
  file = here("inputs/data/inst_data_cleaned.csv"),
  show_col_types = FALSE
)


#### Test data ####



# Test 1: check that there are  0 minimum maledown/femaledown in inst_data_cleaned
inst_data_cleaned$maledown_fn |> min() == 0
inst_data_cleaned$femaledown_fn |> min() == 0


# Test 2: check that the std dataset's num_class variable and see if it has minium 0 class and maximum 5 classes
std_data_cleaned$num_class |> min() == 0
std_data_cleaned$num_class |> max() == 5



# Test 3: check that the std dataset's consider_regrade variable has three unique datanames only 
std_data_cleaned$consider_regrade |> unique() |> length() == 3



# Test 4: check that there are exactly 1295 observations in the std dataset without NA as its datapoints
std_data_cleaned$participantcode|> unique() |> length() == 1295



# Test 5: check that there are exactly 2 options of "1" and "0" for gender in the std dataset using the std_male variable
std_data_cleaned$std_male |> unique() |> length() == 2



# Test 6: check that there are 127 observations in the inst_data_cleaned dataset
inst_data_cleaned$id |> unique() |> length() == 127


# Test 7: check that there are maximum 100 maleup/femaleup and 0 minimum maleup/femaleup in inst_data_cleaned
inst_data_cleaned$maleup_fn |> min() == 0
inst_data_cleaned$maleup_fn  |> max() == 100

inst_data_cleaned$femaleup_fn |> min() == 0
inst_data_cleaned$femaleup_fn  |> max() == 100


# Test 8: check that there are minimum malesame/femalesame in inst_data_cleaned
inst_data_cleaned$malesame_fn |> min() == 0
inst_data_cleaned$femalesame_fn |> min() == 0




