library(dplyr)
library(ggplot2)


prison_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")


str(prison_data)
nrow(prison_data)
ncol(prison_data)


latest_year <- max(prison_data$year, na.rm = TRUE)


total_prison_pop_latest <- prison_data %>%
  filter(year == latest_year) %>%
  summarise(total_prison_pop = sum(total_pop, na.rm = TRUE)) %>%
  pull(total_prison_pop)
total_prison_pop_latest


avg_prison_pop_per_county <- prison_data %>%
  filter(year == latest_year) %>%
  summarise(avg_prison_pop = mean(total_pop, na.rm = TRUE)) %>%
  pull(avg_prison_pop)
avg_prison_pop_per_county


state_highest_prison_pop <- prison_data %>%
  filter(year == latest_year) %>%
  group_by(state) %>%
  summarise(total_prison_pop = sum(total_pop, na.rm = TRUE)) %>%
  arrange(desc(total_prison_pop)) %>%
  slice(1) %>%
  pull(state)
state_highest_prison_pop


change_prison_pop_last_10_years <- prison_data %>%
  filter(year %in% c(latest_year, latest_year - 10)) %>%
  group_by(year) %>%
  summarise(total_prison_pop = sum(total_pop, na.rm = TRUE)) %>%
  summarise(change = diff(total_prison_pop)) %>%
  pull(change)
change_prison_pop_last_10_years


black_prison_pop <- prison_data %>%
  filter(year == latest_year) %>%
  summarise(black_prison_pop = sum(black_pop_15to64, na.rm = TRUE)) %>%
  pull(black_prison_pop)
black_prison_pop


black_prison_pop_percentage <- (black_prison_pop / total_prison_pop_latest) * 100
black_prison_pop_percentage

# Print all the results
cat("Total prison population in the latest year:", total_prison_pop_latest, "\n")
cat("Average prison population per county in the latest year:", avg_prison_pop_per_county, "\n")
cat("State with the highest prison population:", state_highest_prison_pop, "\n")
cat("Change in prison population over the last 10 years:", change_prison_pop_last_10_years, "\n")
cat("Percentage of incarcerated individuals who are Black in the latest year:", black_prison_pop_percentage, "%", "\n")
