library(dplyr)
library(ggplot2)
library(tidyr)
library(viridis)


prison_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")


latest_year <- max(prison_data$year, na.rm = TRUE)


latest_year_data <- prison_data %>%
  filter(year == latest_year)


latest_year_data <- latest_year_data %>%
  mutate(total_pop_15to64 = rowSums(select(., aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64, white_pop_15to64), na.rm = TRUE))


total_race_populations <- latest_year_data %>%
  summarise(total_aapi_pop = sum(aapi_pop_15to64, na.rm = TRUE),
            total_black_pop = sum(black_pop_15to64, na.rm = TRUE),
            total_latinx_pop = sum(latinx_pop_15to64, na.rm = TRUE),
            total_native_pop = sum(native_pop_15to64, na.rm = TRUE),
            total_white_pop = sum(white_pop_15to64, na.rm = TRUE),
            total_pop_15to64 = sum(total_pop_15to64, na.rm = TRUE))


total_race_populations <- total_race_populations %>%
  mutate(aapi_percentage = (total_aapi_pop / total_pop_15to64) * 100,
         black_percentage = (total_black_pop / total_pop_15to64) * 100,
         latinx_percentage = (total_latinx_pop / total_pop_15to64) * 100,
         native_percentage = (total_native_pop / total_pop_15to64) * 100,
         white_percentage = (total_white_pop / total_pop_15to64) * 100)


total_race_populations_long <- total_race_populations %>%
  pivot_longer(cols = c(aapi_percentage, black_percentage, latinx_percentage, native_percentage, white_percentage),
               names_to = "race",
               values_to = "percentage")


ggplot(total_race_populations_long, aes(x = race, y = percentage, fill = race)) +
  geom_bar(stat = "identity") +
  labs(title = "Percentage of Different Racial Groups in the Total Population Aged 15 to 64",
       x = "Race",
       y = "Percentage of Total Population Aged 15 to 64",
       fill = "Race") +
  theme_minimal() +
  scale_fill_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


