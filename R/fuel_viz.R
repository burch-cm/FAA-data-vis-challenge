
isnt_out_mad <- function(x, thres = 3, na.rm = TRUE) {
  abs(x - median(x, na.rm = na.rm)) <= thres * mad(x, na.rm = na.rm)
}

fuel <- readRDS("./data/fur_2015_2021.rds") |> 
  tidyr::drop_na(date) |> 
  dplyr::filter(date >= as.Date("2019-10-01")) |> 
  # remove negative values
  dplyr::mutate(units = abs(units)) |> 
  # remove outliers over 3sd away from median (MAD: Med Abs Dev)
  dplyr::filter(isnt_out_mad(units))  
  
dplyr::glimpse(fuel)
summary(fuel)

fuel_xwalk <- 
  tibble::tibble(purchase_fuel = c("GAS", "E85", "DSL", 
                                   "B20", "CNG", "LPG",
                                   "ELE"),
                 fuel_label = c("Unleaded<br>Gasoline", "Ethanol (E85)", "Diesel",
                                "BioDiesel (B20)", "Compressed<br>Natural Gas",
                                "Liquified<br>Petroleum Gas", "Electric"),
                 fuel_class = c("Petroleum", "Alternative", "Petroleum",
                                "Alternative", "Alternative", "Alternative",
                                "Electric"))

##### DyGraph #####

fuel_mx <-
  fuel |> 
  select(date, purchase_fuel, units) |>
  left_join(fuel_xwalk, by = 'purchase_fuel') |> 
  count(date, fuel_class, name = 'units', )
  group_by(date, fuel_class) |> 
  summarize(units = sum(units), .groups = 'drop') |> 
  pivot_wider(id_cols = date, names_from = fuel_class, values_from = units) |> 
  select(date, Petroleum, Alternative, Electric)

fuel_matrix <-
  fuel_mx |> 
  select(Petroleum, Alternative, Electric) |> 
  xts::xts(order.by = fuel_mx$date)

library(dygraphs)
dygraph(fuel_matrix) |> 
  dySeries('Petroleum') |> 
  dySeries('Alternative') |> 
  dySeries('Electric') |> 
  dyOptions(#colors = RColorBrewer::brewer.pal(3, 'Set2'),
            colors = c("maroon", "orange", "green"),
            stepPlot = TRUE,
            fillGraph = TRUE,
            fillAlpha = 0.5,
            drawGrid = FALSE) |>
  # dyHighlight(highlightCircleSize = 2,
  #             highlightSeriesBackgroundAlpha = 0.2,
  #             hideOnMouseOut = FALSE) |> 
  dyAxis('y', label = "Units (GGE)") # |> 
  # dyRangeSelector(height = 20)


##### plotly series #####
library(tidyverse)
library(plotly)

fig <- 
  fuel |> 
  select(date, purchase_fuel, units) |> 
  left_join(fuel_xwalk, by = 'purchase_fuel') |> 
  drop_na(purchase_fuel) |> 
  group_by(date, fuel_class) |> 
  summarize(units = sum(units, na.rm = TRUE), .groups = 'drop') |> 
  pivot_wider(id_cols = date, values_from = units, names_from = fuel_class) |> 
  replace_na(list(Alternative = 0, Petroleum = 0, Electric = 0)) |> 
  select(date, Petroleum, Alternative) |> 
  plot_ly(x = ~ date) |> 
  add_trace(y = ~ Petroleum, 
            name = 'Petroleum',
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'rgb(128, 0, 0)')) |> 
  add_trace(y = ~ Alternative, 
            name = 'Alternative',
            type = 'scatter', 
            mode = 'lines',
            line = list(color = 'rgb(255, 165, 0)'))

fig |> layout(
  title = "Fuel Purchased",
  xaxis = list(
    # rangeselector = list(
    #   buttons = list(
    #     list(
    #       count = 3,
    #       label = "3 mo",
    #       step = "month",
    #       stepmode = "backward"),
    #     list(
    #       count = 6,
    #       label = "6 mo",
    #       step = "month",
    #       stepmode = "backward"),
    #     list(
    #       count = 1,
    #       label = "1 yr",
    #       step = "year",
    #       stepmode = "backward"),
    #     list(
    #       count = 1,
    #       label = "YTD",
    #       step = "year",
    #       stepmode = "todate"),
    #     list(step = "all"))),
    # 
    rangeslider = list(type = "date")),
  
  yaxis = list(title = "gallons purchased"))

##### plotly sunburst #####

library(plotly)

# plotly::plot_ly(
#   labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
#   parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
#   values = c(10, 14, 12, 10, 2, 6, 6, 4, 4),
#   type = 'sunburst'
# )
# tibble(
#   labels = c("Eve", "Cain", "Seth", "Enos", "Noam", "Abel", "Awan", "Enoch", "Azura"),
#   parents = c("", "Eve", "Eve", "Seth", "Seth", "Eve", "Eve", "Awan", "Eve"),
#   values = c(10, 14, 12, 10, 2, 6, 6, 4, 4),
#   type = 'sunburst'
# ) |> 
#   plot_ly(labels = ~ labels,
#           parents = ~ parents,
#           values = ~ values,
#           type = 'sunburst')


# tibble(
#   labels = c("Petroleum", "Alternative", "B20", "CNG", "DSL", "E85", "DSL", "GAS"),
#   parents = c("", "", "Alternative", "Alternative", "Petroleum", "Alternative",
#               "Petroleum", "Petroleum"),
#   values = c(13, 11, 4, 5, 6, 2, 3, 4)
# ) |> 
#   plot_ly(labels = ~ labels, 
#           parents = ~ parents,
#           values = ~ values, 
#           type = 'sunburst')


fuel_class_sum <- 
  fuel |> 
  filter(date >= as.Date("2020-10-01")) |> 
  select(date, purchase_fuel, units) |>
  left_join(fuel_xwalk, by = 'purchase_fuel') |> 
  group_by(purchase_fuel, fuel_class, fuel_label) |> 
  summarise(units = sum(units, na.rm = TRUE), .groups = 'drop') |> 
  filter(!is.na(purchase_fuel) & purchase_fuel != 'ELE') |> 
  select(fuel_label, fuel_class, units) |> 
  group_by(fuel_class) |> 
  summarize(units = sum(units), .groups = 'drop') |> 
  rename(fuel_label = fuel_class) |> 
  mutate(fuel_class = "")

fuel_plt <- 
  fuel |> 
  filter(date >= as.Date("2020-10-01")) |> 
  select(date, purchase_fuel, units) |>
  left_join(fuel_xwalk, by = 'purchase_fuel') |> 
  group_by(purchase_fuel, fuel_class, fuel_label) |> 
  summarise(units = sum(units, na.rm = TRUE), .groups = 'drop') |> 
  filter(!is.na(purchase_fuel) & purchase_fuel != 'ELE') |> 
  select(fuel_label, fuel_class, units)
 

fig_fuel <-
  fuel_class_sum |> 
  rbind(fuel_plt) |> 
  plotly::plot_ly(
    labels = ~ fuel_label,
    parents = ~ fuel_class,
    values = ~ round(units, 0),
    type = 'sunburst'
  ) 

fig_fuel |> layout(
  title = "Fuel Purchased",
  xaxis = list(
    
    rangeslider = list(type = "date")),
  
  yaxis = list(title = "gallons purchased"))



##### plotly map #####

# library(plotly)
# state_abb <- c(state.abb, "DC", "AE", "GU", "PR", "AS")
# state_name <- c(state.name, 
#                 "District of Columbia",
#                 "Armed Forces Europe",
#                 "Guam",
#                 "Puerto Rico",
#                 "American Samoa")
# states <- data.frame(state = state_abb, state_name = state_name)
# us_states <- geojsonio::geojson_read("json/gz_2010_us_040_00_5m.json", what = "sp")
# 
# g <- list(
#   scope = 'usa',
#   projection = list(type = 'albers usa'),
#   showlakes = FALSE,
#   lakecolor = toRGB('white')
# )

# fuel |> 
#   mutate(FY = lubridate::year(date),
#          FY = if_else(lubridate::month(date) >= 10, FY + 1, FY),
#          month = lubridate::month(date, label = TRUE, abb = TRUE),
#          purchase_fuel = as.factor(purchase_fuel)) |> 
#   group_by(state, FY, month, purchase_fuel) |> 
#   summarize(units = sum(units), .groups = "keep") |> 
#   left_join(states, by = 'state') |> 
#   readr::write_rds("./data/fuel_by_state.rds")  

#   filter(purchase_fuel %in% c('GAS', 'DSL', 'E85')) |> 
#   pivot_wider(id_cols = c(state, FY, state_name),
#               names_from = purchase_fuel,
#               values_from = units) |> 
#   mutate(hover = paste(state_name, '<br>',
#                        'GAS:', GAS, '<br>',
#                        'DSL:', DSL, '<br>',
#                        'E85:', E85)) |> 
#   pivot_longer(cols = c('DSL', 'E85', 'GAS'),
#                names_to = 'purchase_fuel',
#                values_to = 'units')
# summary(f85)
# glimpse(f85)

# plot_geo({f85 |> 
#           filter(FY == "2020") |> 
#           filter(purchase_fuel == "E85")}, 
#          locationmode = "USA-states") |> 
#   add_trace(z = ~ units, locations = ~ state) |> 
#   layout(geo = g,
#          title = "E85 Purchases by State, FY2020")
