library(tidyverse) |> suppressPackageStartupMessages()
library(ggthemes)

crash <- vroom::vroom("./data/crashdata.csv", delim = ",")

glimpse(crash)

crash |> 
  mutate(FQ = ordered(quarter)) |> 
  group_by(FQ) |> 
  ggplot(aes(x = FQ)) +
  geom_bar(aes(fill = incident_accident)) +
  guides(fill = guide_legend(title = "Event Type")) +
  labs(title = "Events by Fiscal Quarter", 
       x = "Fiscal Quarter", 
       y = "Count of Events") +
  theme_tufte()

crash |> 
  group_by(quarter, incident_accident) |> 
  count() |> 
  summarize(last = last(quarter))

crash |> 
  mutate(FQ = ordered(quarter)) |>
  ggplot(aes(x = FQ)) +
  geom_bar(aes(fill = incident_accident)) +
  guides(fill = guide_legend(title = "Event Type")) +
  labs(title = "Events by Fiscal Quarter", 
       x = "Fiscal Quarter", 
       y = "Count of Events") +
  theme_tufte() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
