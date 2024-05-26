library(dplyr)
library(ggplot2)



prison_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")


latest_year <- max(prison_data$year, na.rm = TRUE)


latest_year_data <- prison_data %>%
  filter(year == latest_year)


latest_year_data <- latest_year_data %>%
  mutate(incarceration_rate = (total_pop / total_pop_15to64) * 100000)


state_data <- latest_year_data %>%
  group_by(state) %>%
  summarise(total_prison_pop = sum(total_pop, na.rm = TRUE),
            incarceration_rate = mean(incarceration_rate, na.rm = TRUE))


ggplot(state_data, aes(x = incarceration_rate, y = total_prison_pop)) +
  geom_point(aes(color = state), size = 3) +
  labs(title = "Relationship Between Incarceration Rate and Total Prison Population",
       x = "Incarceration Rate per 100,000 People",
       y = "Total Prison Population",
       color = "State") +
  theme_minimal()


