library(tidyverse)
dat <- read_csv("./rawdata/prod_administratorsfactbook_faaminorities.csv") |> 
  rename(index = `_c0`, 
         LOB_name = lineofbusiness, 
         percent_minority_2020 = `_2020minority`, 
         percent_non_minority_2020 = `_2020non_minority`) |> 
  select(-index) |> 
  mutate(LOB = stringr::word(LOB_name, -1),
         LOB = stringr::str_remove_all(LOB, "[()]"))

avg_percent_minority <-
  dat |> 
  filter(LOB == 'Total') |> 
  pull(percent_minority_2020)

dat |> 
  filter(LOB != "Total") |> 
  mutate(above_avg = if_else(percent_minority_2020 > avg_percent_minority, TRUE, FALSE)) |> 
  ggplot(aes(x = LOB, y = percent_minority_2020)) +
  geom_bar(stat = "identity", aes(fill = above_avg)) +
  labs(x = "Line of Business (LOB)", y = "minority %") +
  geom_hline(yintercept = avg_percent_minority) +
  guides(fill = 'none') +
  scale_y_continuous(limits = c(0, 1)) +
  theme_bw() +
  coord_flip()
