#### Preamble ####
# Purpose: Simulates dataset of correlation between gender identities and regrades among undergraduate students, 2010-2016 
# Author: Quang Mai, Catherine Punnoose and Faiza Imam
# Date: 12 February 2024 
# Contact: catherine.punnoose@mail.utoronto.ca, q.mai@mail.utoronto.ca, faiza.imam@mail.utoronto.ca
# License: MIT


#### Data Expectations ####
# Number of classes that students  events divided by gender and race
# Expect different types of clinics, like city-run, hospital, pharmacy, pop-ups
# Opening date should be a valid date before today
# columns: clinic_id, district, type, opening_date

#### Workspace setup ####
library(tidyverse)
library(janitor)


#### Simulate the number of classes students considered for regrade requests by students' gender 
# Code referenced from from: https://tellingstorieswithdata.com/02-drinking_from_a_fire_hose.html#simulate

set.seed(97) #ensure simulated data's reproducibility

simulated_data <-
  tibble(
    #use 1 through 65276 to represent each arrest event
    "id" = 1:3885,
    #randomly pick a the number of classes identified from the dataset, with replacement, 65276 times
    "num_class" = sample(
      x = c("0 class", "1 class", "2 classes", "3 classes", "4 classes", "5 classes"),
      size = 3885,
      replace = TRUE
    ),
    #randomly pick one gender identity identified from the dataset, with replacement, 65276 times
    "gender" = sample(
      x = c("Male", "Female"),
      size = 3885,
      replace = TRUE
    ),
    #randomly pick whether the student considers regrade, with replacement, 65276 times
    "consider_regrade" = sample(
      x = c("Yes", "No"),
      size = 3885,
      replace = TRUE
    )
  )


#clean the dataset into one with a percentage column four new columns items_found, reason_injury, reason_escape, reason_weapons and reason_possess_evidence to those events with strip search, with replacement, 32717 times (32717 is based on the set.seed() number)

set.seed(97)

simulated_data <- simulated_data |>
  group_by(num_class, gender, consider_regrade) |>
  count() |>
  group_by(gender, consider_regrade) |>
  mutate(percentage = round(n/sum(n)*100, 1))

# Generate according tables and graphs
  
set.seed(97)

simulated_data <- simulated_data |>
  filter(consider_regrade != "No")
  
head(simulated_data, 12) |>
  kable(
    col.names = c("Number of Classes", "Gender", "Consider Regrade", "Count", "Percentage"),
    booktabs = TRUE,
    align = c("c", "c", "c", "c", "c")
  )

set.seed(97)

simulated_data |>
  ggplot(aes(x = num_class, y = percentage, fill = gender)) + 
  geom_bar(stat="identity", position = "dodge") +
  theme_minimal() +
  theme(legend.key.size = unit(0.5, 'cm')) +
  geom_text(aes(label=percentage), position=position_dodge(width=0.9), vjust=-0.25) +
  labs(x = "Classes",
       y = "Distribution by gender (percent)") +
  scale_fill_manual(values = c("Male" = "#4B659D", "Female" = "#A83B48")) +
  scale_x_discrete(labels = c("0 class", "1 class", "2 classes", "3 classes", "4 classes", "5+ classes"))

#### Simulate the regrade results by timing and students' gender

set.seed(97) 

simulated_data_regrade <-
  tibble(
    #use 1 through 190 to represent each arrest event
    "id" = 1:190,
    #randomly pick the percentage of increased regrade for male end of semester, with replacement, 190 times
    "maleup_fn" =  runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of decreased regrade for male male end of semester, with replacement, 190 times
    "maledown_fn" = runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of increased regrade for female male end of semester, with replacement, 190 times
    "femaleup_fn" =  runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of decreased regrade for female male end of semester, with replacement, 190 times
    "femaledown_fn" = runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of unchanged regrade for female male end of semester, with replacement, 190 times
    "femalesame_fn" =  runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of unchaged regrade for male male end of semester, with replacement, 190 times
    "malesame_fn" = runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of increased regrade for male during semester, with replacement, 190 times
    "maleup_mt" =  runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of decreased regrade for male male during semester, with replacement, 190 times
    "maledown_mt" = runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of increased regrade for female male during semester, with replacement, 190 times
    "femaleup_mt" =  runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of decreased regrade for female male during semester, with replacement, 190 times
    "femaledown_mt" = runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of unchanged regrade for female male during semester, with replacement, 190 times
    "femalesame_mt" =  runif(n = 190, min = 0, max = 100) |> floor(),
    #randomly pick the percentage of unchaged regrade for male male during semester, with replacement, 190 times
    "malesame_mt" = runif(n = 190, min = 0, max = 100) |> floor()
  )

# Calculate the mean for each category and generate according dataframes

set.seed(97)
femaleup_fn_avg <- mean(simulated_data_regrade$femaleup_fn, na.rm = TRUE)
femalesame_fn_avg <- mean(simulated_data_regrade$femalesame_fn, na.rm = TRUE)
femaledown_fn_avg <- mean(simulated_data_regrade$femaledown_fn, na.rm = TRUE)

maleup_fn_avg <- mean(simulated_data_regrade$maleup_fn, na.rm = TRUE)
malesame_fn_avg <- mean(simulated_data_regrade$malesame_fn, na.rm = TRUE)
maledown_fn_avg <- mean(simulated_data_regrade$maledown_fn, na.rm = TRUE)

femaleup_mt_avg <- mean(simulated_data_regrade$femaleup_mt, na.rm = TRUE)
femalesame_mt_avg <- mean(simulated_data_regrade$femalesame_mt, na.rm = TRUE)
femaledown_mt_avg <- mean(simulated_data_regrade$femaledown_mt, na.rm = TRUE)

maleup_mt_avg <- mean(simulated_data_regrade$maleup_mt, na.rm = TRUE)
malesame_mt_avg <- mean(simulated_data_regrade$malesame_mt, na.rm = TRUE)
maledown_mt_avg <- mean(simulated_data_regrade$maledown_mt, na.rm = TRUE)



simulate_dataframe <- data.frame(
  female_fn = c(femaleup_fn_avg, femalesame_fn_avg, femaledown_fn_avg),
  male_fn = c(maleup_fn_avg, malesame_fn_avg, maledown_fn_avg),
  female_mt = c(femaleup_mt_avg, femalesame_mt_avg, femaledown_mt_avg),
  male_mt = c(femaleup_mt_avg, femalesame_mt_avg, femaledown_mt_avg),
  category = c("Grade Increase", "No Change", "Grade Decrease")
)

head(simulate_dataframe, 3) |>
  kable(
    col.names = c("female_fn", "male_fn", "female_mt", "male_mt", "category"),
    booktabs = TRUE,
    align = c("c", "c", "c", "c")
  )

# Generate according graphs 

#female_fn
ggplot(simulate_dataframe, aes(x = category, y = female_fn, fill = category)) + ylim(0, 90) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(  female_fn )), vjust = -0.5, size = 3) +  # Add labels on top of bars
  labs(title = "Panel B. End of semester, female students",
       x = "Category",
       y = "Percent") + scale_fill_manual(values = c("Grade Increase" = "#4B659D", "No Change" = "#A83B48", "Grade Decrease" = "#B7BA9F")) +  # Adjust colors
  scale_y_continuous(breaks = custom_breaks) +  # Set custom breaks for y-axis
  theme_minimal()

#male_fn
ggplot(simulate_dataframe, aes(x = category, y = female_fn, fill = category)) + ylim(0, 90) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(  female_fn )), vjust = -0.5, size = 3) +  # Add labels on top of bars
  labs(title = "Panel B. During of semester, male students",
       x = "Category",
       y = "Percent") + scale_fill_manual(values = c("Grade Increase" = "#4B659D", "No Change" = "#A83B48", "Grade Decrease" = "#B7BA9F")) +  # Adjust colors
  scale_y_continuous(breaks = custom_breaks) +  # Set custom breaks for y-axis
  theme_minimal()

#female_mt
ggplot(simulate_dataframe, aes(x = category, y = female_mt, fill = category)) + ylim(0, 90) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(female_mt)), vjust = -0.5, size = 3) +  # Add labels on top of bars
  labs(title = "Panel B. End of semester, female students",
       x = "Category",
       y = "Percent") + scale_fill_manual(values = c("Grade Increase" = "#4B659D", "No Change" = "#A83B48", "Grade Decrease" = "#B7BA9F")) +  # Adjust colors
  scale_y_continuous(breaks = custom_breaks) +  # Set custom breaks for y-axis
  theme_minimal()

#male_mt
ggplot(simulate_dataframe, aes(x = category, y = male_mt, fill = category)) + ylim(0, 90) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = round(male_mt)), vjust = -0.5, size = 3) +  # Add labels on top of bars
  labs(title = "Panel B. End of semester, male students",
       x = "Category",
       y = "Percent") + scale_fill_manual(values = c("Grade Increase" = "#4B659D", "No Change" = "#A83B48", "Grade Decrease" = "#B7BA9F")) +  # Adjust colors
  scale_y_continuous(breaks = custom_breaks) +  # Set custom breaks for y-axis
  theme_minimal()

#### Data Validation ####

# Code referenced from: https://tellingstorieswithdata.com/02-drinking_from_a_fire_hose.html

# Check that there are between 1 and 6 events of student regrades for each number of classes 
simulated_data$num_class |> unique() |> length() == 6

# Check that there are exactly 1 option for consider regrade which is "Yes" #
simulated_data$consider_regrade |> unique() |> length() == 1

# Check that there are exactly 2 options for gender which is "Female" and "Male" #
simulated_data$gender |> unique() |> length() == 2

# Check that there are exactly 2 options for gender which is "Female" and "Male" in the dataframe that accounts for number of classes #
simulate_dataframe$category |> unique() |> length() == 3