#### Preamble ####
# Purpose: Downloads and saves survey data to new file
# Author: Faiza Imam, Catherine Punnoose, Quang Mai 
# Data: survey.dta
# Contact: catherine.punnoose@mail.utoronto.ca, q.mai@mail.utoronto.ca, faiza.imam@mail.utoronto.ca
# License: MIT
# Pre-requisites: Have downloaded survey.dta from https://doi.org/10.7910/DVN/QXJDJ5

#loading packages
library(readstata13)
library(readr)

#reading in the stata file
instsurvey <- read.dta13(here::here("inputs/data/raw data/instsurvey.dta"))
stdsurvey <- read.dta13(here::here("inputs/data/raw data/stdsurvey.dta"))

#writing to csv
write_csv(instsurvey, here::here("inputs/data/raw data/instsurvey.csv"))
write_csv(stdsurvey, here::here("inputs/data/raw data/stdsurvey.csv"))
