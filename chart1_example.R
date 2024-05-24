library(dplyr)
library(ggplot2)

prison_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")



latest_year <- max(prison_data$year, na.rm = TRUE)


top_5_states <- prison_data %>%
  filter(year == latest_year) %>%
  group_by(state) %>%
  summarise(total_prison_pop = sum(total_pop, na.rm = TRUE)) %>%
  arrange(desc(total_prison_pop)) %>%
  slice(1:5) %>%
  pull(state)

top_5_states_data <- prison_data %>%
  filter(state %in% top_5_states)


state_prison_pop_trend <- top_5_states_data %>%
  group_by(year, state) %>%
  summarise(total_prison_pop = sum(total_pop, na.rm = TRUE)) %>%
  ungroup()

# Plot the trend
ggplot(state_prison_pop_trend, aes(x = year, y = total_prison_pop, color = state)) +
  geom_line(size = 1.2) +
  labs(title = "Trend of Total Prison Population Over Time for Top 5 States",
       x = "Year",
       y = "Total Prison Population",
       color = "State") +
  theme_minimal()

