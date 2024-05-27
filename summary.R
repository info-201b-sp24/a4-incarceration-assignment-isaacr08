library(mapproj)
library(dplyr)
library(maps)
library(usdata)
library(ggplot2)
library(plotly)

incarceration_trends <- read.csv("incarceration_trends.csv")

# The state with the highest ratio of Latinx jail population count to White jail population count
state_highest_jail_race_ratio <- incarceration_trends %>% 
  select(state, white_jail_pop, latinx_jail_pop) %>% 
  group_by(state) %>% 
  summarise(white_jail_pop = sum(white_jail_pop, na.rm = TRUE),
            latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE)) %>% 
  mutate(rate = latinx_jail_pop / white_jail_pop) %>% 
  filter(rate == max(rate, na.rm = TRUE)) %>% 
  pull(state)

# The state with the highest ratio of Latinx prison population count to White prison population count
state_highest_prison_race_ratio <- incarceration_trends %>% 
  select(state, white_prison_pop, latinx_prison_pop) %>% 
  group_by(state) %>% 
  summarise(white_prison_pop = sum(white_prison_pop, na.rm = TRUE),
            latinx_prison_pop = sum(latinx_prison_pop, na.rm = TRUE)) %>% 
  mutate(rate = latinx_prison_pop / white_prison_pop) %>% 
  filter(rate == max(rate, na.rm = TRUE)) %>% 
  pull(state)

# The state with the highest total incarceration count of Latinx individuals
state_highest_latinx_incarceration <- incarceration_trends %>% 
  select(state, latinx_jail_pop, latinx_prison_pop) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_latinx_incarceration = latinx_jail_pop + latinx_prison_pop) %>% 
  group_by(state) %>% 
  summarise(total_incarceration = sum(total_latinx_incarceration, na.rm = TRUE)) %>% 
  filter(total_incarceration == max(total_incarceration, na.rm = TRUE)) %>% 
  pull(state)

# The highest total incarceration count of Latinx individuals in a state
highest_latinx_incarceration_count <- incarceration_trends %>% 
  select(state, latinx_jail_pop, latinx_prison_pop) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_latinx_incarceration = latinx_jail_pop + latinx_prison_pop) %>% 
  group_by(state) %>% 
  summarise(total_incarceration = sum(total_latinx_incarceration, na.rm = TRUE)) %>% 
  filter(total_incarceration == max(total_incarceration, na.rm = TRUE)) %>% 
  summarise(rounded_total = round(total_incarceration)) %>% 
  pull(rounded_total)

# Percentage change of Latinx jail incarceration over the last 10 years (2008-2018)
latinx_jail_change_year <- incarceration_trends %>% 
  filter(year %in% c(2008, 2018)) %>% 
  select(year, latinx_jail_pop) %>% 
  group_by(year) %>% 
  summarise(latinx_jail_pop = sum(latinx_jail_pop, na.rm = TRUE)) %>% 
  summarise(change = (latinx_jail_pop[2] - latinx_jail_pop[1]) / latinx_jail_pop[1]) %>% 
  mutate(change_decimal = round(change, 2)) %>% 
  pull(change_decimal)

# Percentage change of Latinx prison incarceration over the last 10 years (2006-2016)
latinx_prison_change_year <- incarceration_trends %>% 
  filter(year %in% c(2006, 2016)) %>% 
  select(year, latinx_prison_pop) %>% 
  group_by(year) %>% 
  summarise(latinx_prison_pop = sum(latinx_prison_pop, na.rm = TRUE)) %>% 
  summarise(change = (latinx_prison_pop[2] - latinx_prison_pop[1]) / latinx_prison_pop[1]) %>% 
  mutate(change_decimal = round(change, 2)) %>% 
  pull(change_decimal)

# Trends over time chart
top3_state_latinx_incarceration <- incarceration_trends %>% 
  select(year, state, latinx_jail_pop, latinx_prison_pop) %>%
  replace(is.na(.), 0) %>% 
  mutate(total_latinx_incarceration = latinx_jail_pop + latinx_prison_pop) %>%
  group_by(year, state) %>% 
  summarise(total_incarceration = sum(total_latinx_incarceration, na.rm = TRUE)) %>% 
  filter(state %in% c("CA", "TX", "NY"))

trends_over_time <- plot_ly(
  data = top3_state_latinx_incarceration,
  x = ~year,
  y = ~total_incarceration,
  color = ~state,
  type = "bar",
  width = 1000
) %>% 
  layout(
    title = "Total Incarcerated Latinx Population in Top 3 States over the Years",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Total Incarceration Number")
  )

# Variable comparison chart
latinx_white_incarceration <- incarceration_trends %>% 
  select(year, latinx_jail_pop, latinx_prison_pop, white_jail_pop, white_prison_pop) %>%
  replace(is.na(.), 0) %>% 
  mutate(latinx_sum = latinx_jail_pop + latinx_prison_pop, 
         white_sum = white_jail_pop + white_prison_pop) %>% 
  group_by(year) %>%
  summarise(latinx_sum = sum(latinx_sum, na.rm = TRUE), white_sum = sum(white_sum, na.rm = TRUE))

variable_comparison <- plot_ly(
  data = latinx_white_incarceration,
  x = ~year,
  y = ~latinx_sum,
  name = "Latinx",
  type = 'scatter', 
  mode = 'lines',
  width = 800
) %>%
  add_trace(
    y = ~white_sum,
    name = "White",
    mode = 'lines'
  ) %>% 
  layout(
    title = "Latinx vs. White Total Incarceration Count over the Years",
    xaxis = list(title = "Year"),
    yaxis = list(title = "Total Incarceration Number")
  )


# Calculate the Latinx-to-White incarceration ratio by state
state_latinx_white_ratio <- incarceration_trends %>% 
  select(state, latinx_jail_pop, latinx_prison_pop, white_jail_pop, white_prison_pop) %>% 
  replace(is.na(.), 0) %>% 
  group_by(state) %>% 
  mutate(latinx_sum = latinx_jail_pop + latinx_prison_pop, 
         white_sum = white_jail_pop + white_prison_pop) %>% 
  summarise(latinx_sum = sum(latinx_sum), white_sum = sum(white_sum)) %>% 
  mutate(rate = latinx_sum / white_sum) %>% 
  select(-latinx_sum, -white_sum) %>% 
  arrange(-rate) %>% 
  slice(-c(1)) # Assuming you want to remove the first row (if it's an outlier or not needed)

# Prepare state shape data and merge with the incarceration ratio
state_shape <- map_data("state") %>%
  rename(state = region)

state_shape <- state_shape %>%
  mutate(state = tolower(state),
         state = state2abbr(state))

# Merge state shape data with the calculated incarceration ratio
state_shape <- state_shape %>%
  left_join(state_latinx_white_ratio, by = "state")

# Plot the map
map <- ggplot(state_shape) +
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group, fill = rate),
    color = "white",
    size = 0.1
  ) +
  coord_map() + 
  scale_fill_continuous(low = "#132B43", high = "Red", na.value = "grey50") +
  labs(fill = "Ratio") +
  ggtitle("Latinx-to-White Incarceration Ratio") +
  theme_bw() +
  theme(
    axis.line = element_blank(), 
    axis.text = element_blank(), 
    axis.ticks = element_blank(), 
    axis.title = element_blank(), 
    plot.background = element_blank(), 
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(), 
    panel.border = element_blank() 
  )

# Display the map
print(map)

# Display the trends over time chart
print(trends_over_time)

# Display the variable comparison chart
print(variable_comparison)
