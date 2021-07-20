library(pacman)
pacman::p_load(tidyverse, vroom, plotly)

# crosswalk state names to abbreviations
state_data <- 
  tibble(state = state.name) |> 
  bind_cols(tibble(abb = state.abb)) |> 
  bind_rows(tibble(state = c("Dist of Columbia",
                             "Americas",
                             "Europe and Canada",
                             "American Samoa",
                             "Guam",
                             "Marshall Islands",
                             "Northern Mariana Island",
                             "Puerto Rico",
                             "Virgin Islands",
                             "Middle East",
                             "Pacific"),
                   abb = c("DC", "AA", "AE", "AS", "GU", "MH", "MP", "PR", 
                           "VI", "AE", "AP")))


pilots <- 
  vroom("./rawdata/activepilotsbystate11012020.csv",
        delim = ",") |> 
  janitor::clean_names() |> 
  mutate(state = str_remove_all(state, ".{2}-")) |> 
  mutate(state = stringr::str_to_title(state),
         state = str_replace_all(state, "Of", "of"),
         state = str_replace_all(state, "And", "and")) |> 
  left_join(state_data, by = "state") |> 
  mutate(hover = paste(state, "<br>", 
                       "Student Pilots:", student_pilot, "<br>",
                       "Sport Pilots:", sport_pilot, "<br>",
                       "Recreational Pilots:", rec_pilot, "<br>",
                       "Commercial Pilots:", com_pilot, "<br>",
                       "ATP Pilots:", atp_pilot))
write_rds(pilots, "./data/us_pilot_count.rds")
glimpse(pilots) 

# plot
# set up options
g <- list(scope = 'usa',
          projection = 'albers usa',
          showlakes = FALSE,
          lakecolor = toRGB('white'))
  
fig <- 
  plot_geo(pilots, locationmode = "USA-states") |> 
  add_trace(z = ~ total_us_pilot,
            text = ~ hover,
            locations = ~ abb) |> 
  colorbar(title = "Registered Pilots", 
           limits = c(0, 80000)) |> 
  layout(title = "Registered Pilots in the United States, 2020",
         geo = g)
fig


#### ggplot
library(tidyverse)
pilots <- read_rds("./data/us_pilot_count.rds")
glimpse(pilots)

pilots |> 
  select(-hover) |> 
  pivot_longer(-c(state, abb), names_to = "pilot_type", values_to = "count") |> 
  filter(pilot_type != "total_us_pilot") |> 
  group_by(pilot_type) |> 
  summarize(count = sum(count)) |> 
  ggplot(aes(x = reorder(pilot_type, count), 
             y = count, 
             label = formattable::comma(count, digits = 0L))) +
  geom_bar(stat = "identity", col = "darkblue", aes(fill = count)) +
  geom_text(hjust = -.1) +
  scale_fill_gradient(low = "greenyellow", high = "forestgreen") +
  theme_bw() +
  scale_y_continuous(labels = scales::comma, limits = c(0, 660000)) +
  labs(x = "Pilot Type", y = "count of pilots", title = "US Pilots") +
  guides(fill = "none") +
  coord_flip()
