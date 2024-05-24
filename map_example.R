
library(dplyr)
library(ggplot2)
library(maps)
library(viridis)


prison_data <- read.csv("https://raw.githubusercontent.com/melaniewalsh/Neat-Datasets/main/us-prison-pop.csv")


latest_year <- max(prison_data$year, na.rm = TRUE)


latest_year_data <- prison_data %>%
  filter(year == latest_year)


state_prison_pop <- latest_year_data %>%
  group_by(state) %>%
  summarise(total_prison_pop = sum(total_pop, na.rm = TRUE))


states_map <- map_data("state")


state_prison_pop$region <- tolower(state.name[match(state_prison_pop$state, state.abb)])
states_map <- left_join(states_map, state_prison_pop, by = "region")


ggplot(states_map, aes(x = long, y = lat, group = group, fill = total_prison_pop)) +
  geom_polygon(color = "white") +
  scale_fill_viridis_c(name = "Total Prison Population", label = scales::comma, na.value = "grey50") +
  theme_minimal() +
  labs(title = "Total Prison Population by State in Latest Year",
       fill = "Total Prison Population",
       x = "",
       y = "") +
  theme(legend.position = "right")
