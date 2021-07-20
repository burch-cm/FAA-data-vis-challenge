library(tidyverse)
library(lubridate)
library(readxl)

fs <-
  read_excel("./rawdata/fleet_stats_summary.xlsx") |>
  pivot_longer(c(Oct:Sep), names_to = "month", values_to = "gge") |>
  left_join(data.frame(m_digit = 1:12, month = month.abb), by = "month") |>
  mutate(type = as.factor(Type),
         month = ordered(month, levels = c(month.abb[10:12], month.abb[1:9])),
         c_year = if_else(m_digit >= 10, FY - 1, FY),
         d_num = paste(m_digit, c_year, sep = "/"),
         date = parse_date_time(d_num, orders = c("m/Y"))) |>
  select(type, FY, month, date, gge)

write_rds(fs, "./data/fuel_stats_summary.rds")

# fs |>
#   drop_na(gge) |>
#   ggplot(aes(x = date, y = gge)) +
#   geom_area(aes(fill = type), position = "stack")

plot_fuel_summary <- function(fs, chart = "area") {
  g <-
    fs |> 
    tidyr::drop_na(gge) |> 
    ggplot2::ggplot(aes(x = date, y = gge, fill = type))
  
  if (chart == "bar") {
    gg <- 
      g + 
      ggplot2::geom_bar(stat = "identity", position = "stack")
  } else if (chart == "area") {
    gg <- 
      g + 
      ggplot2::geom_area(position = "stack")
  }
  gg +
    theme_bw() +
    labs(x = "Purchase Month", y = "Amount Purchased (GGE)") +
    scale_y_continuous(labels = scales::comma)
}

fs |> plot_fuel_summary("area")
fs |> plot_fuel_summary("bar")
