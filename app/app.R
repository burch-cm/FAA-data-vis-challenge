library(dplyr)
library(tidyr)
library(tibble)
library(shiny)
library(shinydashboard)
library(chorddiag)
library(plotly)
library(dygraphs)
library(htmlwidgets)
library(htmltools)

##### preload #####
# values
state_abb <- c(state.abb, "DC", "AE", "GU", "PR", "AS")
state_name <- c(state.name, 
                "District of Columbia",
                "Armed Forces Europe",
                "Guam",
                "Puerto Rico",
                "American Samoa")
states <- data.frame(garage_state = state_abb, state_name = state_name)

fuel_xwalk <- 
  tibble::tibble(purchase_fuel = c("GAS", "E85", "DSL", 
                                   "B20", "CNG", "LPG",
                                   "ELE"),
                 fuel_label = c("Unleaded Gasoline", "Ethanol (E85)", "Diesel",
                                "BioDiesel (B20)", "Compressed Natural Gas",
                                "Liquified Petroleum Gas", "Electric"),
                 class_group = c("Petroleum", "Alternative", "Petroleum",
                                "Alternative", "Alternative", "Alternative",
                                "Electric")) |> 
  mutate(fuel_class = class_group)

fuel_desc <- tibble(fuel_class = as.factor(c("HEV", "GAS", "FEV", 
                                             "E85", "DSL", "B20/CNG")),
                    class_desc = as.factor(c("Gas/Elec Hybrid",
                                             "Gasoline",
                                             "Electric",
                                             "Ethanol (E85)",
                                             "Diesel",
                                             "Other Fuel")))

veh_xwalk <- tibble(
  vehicle_type = c('Hd', 'Med & Heavy Cab And Chassis', 'Ld Suv 4X4', 'Heavy Tractors', 'Ld Suv 4X2', 'Ld Minivan 4X2 (Passenger)', 'Ld Minivan 4X2 (Cargo)', 'Ld Van 4X2 (Passenger)', 'Ld Pickup 4X2', 'Md Pickup', 'Md Van (Cargo)', 'Ld Pickup 4X4', 'Md Other', 'Md Suv', 'Ld Van 4X4 (Passenger)', 'Ld Minivan 4X4 (Passenger)', 'Sedan/St Wgn Compact', 'Sedan/St Wgn Subcompact', 'Ld Van 4X2 (Cargo)', 'Sedan/St Wgn Midsize', 'Md Van (Passenger)', 'Md Bus', 'Hd Bus', 'Sedan', 'Ld Van 4X4 (Cargo)', 'Sedan/St Wagon'),
  vehicle_class = c('Other', 'Other', 'SUV', 'Other', 'SUV', 'Van', 'Van', 'Van', 'Pickup', 'Pickup', 'Van', 'Pickup', 'Other', 'SUV', 'Van', 'Van', 'Sedan', 'Sedan', 'Van', 'Sedan', 'Van', 'Other', 'Other', 'Sedan', 'Van', 'Sedan')
)

##### dashboard UI #####

ui <- dashboardPage(
  
  shinydashboard::dashboardHeader(title = "FAA Fleet"),
  
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      menuItem("Fleet Composition",
               tabName = 'composition',
               icon = icon('car')),
      menuItem("Vehicle Location",
               tabName = 'vehMap',
               icon = icon('map')),
      menuItem("Fuel Use",
               tabName = 'fuelUse',
               icon = icon('gas-pump'))
    )
  ),
  
  shinydashboard::dashboardBody(
    
    tags$head(
      tags$link(rel = 'stylesheet', type = 'text/css', href = 'app_style.css')
    ),
    
    tabItems(
      #### chord tab ####
      tabItem(
        tabName = 'composition',
        fluidRow(
          box(
            chorddiag::chorddiagOutput("inv_chord",
                                         width = "100%",
                                         height = "500px"),
            div(
              selectizeInput("states", label = "States",
                             choices = state_abb,
                             multiple = TRUE, 
                             width = "70%",
                             options = list(plugins = list('remove_button')))
            ),
            width = 6,
            title = "What types of vehicles are in the FAA fleet?"),
          column(width = 6,
                 valueBoxOutput("vehicle_count", width = NULL),
                 fluidRow(
                   valueBoxOutput("ev_count", width = 4),
                   valueBoxOutput("alt_count", width = 4),
                   valueBoxOutput("petro_count", width = 4)
                 ),
                 box(div("Alternative Fuels are defined by the US Dept. of Energy, and 
                          include Ethanol or E85, Biodiesel, Natural Gas, Propane, and 
                          gas-powered Hybrid Electric vehicles."),
                     title = "What types of fuel are considered 'Alternative'?",
                     width = NULL), 
                 box(div("This chart displays information for 'on-road reportable' 
                          vehicles, a category which includes all non-industrial
                          wheeled vehicles authorized to travel on public roadways. 
                          All on-road reportable vehicles have license plates. 
                          This category does not include specialty vehicles used in 
                          construction (e.g., bucket loaders) or vehicles without 
                          an engine (e.g. trailers)."),
                     title = "What types of vehicles are tracked?",
                     width = NULL)
          )
        )
      ),
      
      #### fuel tab ####
      tabItem(tabName = 'fuelUse',
        fluidRow(
          box(plotly::plotlyOutput("fuel_sunburst",
                                   width = "100%",
                                   height = "500px"),
              sliderInput("fuel_dates",
                          "Filter by Date:",
                          min = as.Date("2020-10-01"), # start of FY21
                          max = as.Date("2021-07-19"), # most recent date
                          value = c(as.Date("2020-10-01"),
                                    as.Date("2021-07-19")),
                          timeFormat = "%Y-%m-%d",
                          width = NULL),
              dygraphOutput("dygraph", height = '50px'),
              width = 6,
              title = "What type of fuel does the FAA fleet use?"),
          column(width = 6,
                 valueBoxOutput("total_gge", width = NULL),
                 fluidRow(
                   valueBoxOutput("elec_gge", width = 4),
                   valueBoxOutput("alt_gge", width = 4),
                   valueBoxOutput("petro_gge", width = 4)
                 ),
                 box(div("Gasoline Gallon Equivalent (GGE) is a measure that relates 
                         the energy content of alternative fuels with gasoline. 
                         This allows us to compare the amount of fuel used between 
                         conventional fuel vehicles and alternative fuel vehicles."),
                     title = "What is GGE?",
                     width = NULL), 
                 box(div("This chart displays information for 'on-road reportable' 
                          vehicles, a category which includes all non-industrial
                          wheeled vehicles authorized to travel on public roadways. 
                          All on-road reportable vehicles have license plates. 
                          This category does not include specialty vehicles used in 
                          construction (e.g., bucket loaders) or vehicles without 
                          an engine (e.g. trailers)."),
                     title = "What types of vehicles are tracked?",
                     width = NULL)
          )
        )
      ),
      
       #### map tab ####
      tabItem(tabName = 'vehMap',
        fluidRow(
          box(
            plotlyOutput("vehicle_map", height = "500px", width = "100%"),
            fluidRow(
              column(6,
                     selectizeInput("map_fuel_sel",
                                 label = "Vehicle Fuel Type",
                                 choices = fuel_desc$class_desc,
                                 multiple = TRUE,
                                 options = list(plugins = list('remove_button')))),
              column(6, 
                     selectizeInput("map_type_sel",
                                 label = "Body Type",
                                 choices = veh_xwalk$vehicle_class,
                                 multiple = TRUE,
                                 options = list(plugins = list('remove_button'))))
            ),    
            width = 6,
            title = "Where are FAA vehicles located?"),
          column(width = 6,
                 valueBoxOutput("total_veh", width = NULL),
                 fluidRow(
                   valueBoxOutput("x1", width = 4),
                   valueBoxOutput("x2", width = 4),
                   valueBoxOutput("x3", width = 4)
                 ),
                 box(title = "Explain", width = NULL),
                 box(title = "Another Box", width = NULL)
          )
        )
      )
    )
  )
)

##### dashboard server #####

server <- function(input, output, session) {
  #### load files ####
  # load inventory data
  inv_data <- readr::read_rds("./appdata/fleet_inv.rds") |> 
    left_join(select(fuel_xwalk, purchase_fuel, fuel_label), 
              by = c('fuel_abbr' = 'purchase_fuel'))
  
  # reactive inventory
  sel_inv <- reactive({
    if (is.null(input$states)) {
      inv_data
    } else {
      filter(inv_data, garage_state %in% input$states)
    }
  })
  
  # reactive fuel
  fuel_dat <- reactive({
    if(!is.null(input$fuel_dates[1]) & !is.null(input$fuel_dates[2])) {
      filter(fuel, date >= input$fuel_dates[1] & date <= input$fuel_dates[2])
    } else {
      fuel
    }
    
    # filter(date >= input$dygraph_date_window[[1]] & 
    #        date <= input$dygraph_date_window[[2]])
  })
  
  # count by state with hover text added
  hover_txt <-
    inv_data |>
    # sel_inv() |> 
    count(garage_state, fuel_class, name = "vehicles") |> 
    pivot_wider(id_cols = garage_state, 
                names_from = fuel_class,
                values_from = vehicles,
                values_fill = 0) |> 
    left_join(states, by = "garage_state") |> 
    mutate(desc = paste(state_name, "<br>",
                        "Gasoline Vehicles:", GAS, "<br>",
                        "Ethanol Vehicles:", E85, "<br>",
                        "Diesel Vehicles:", DSL, "<br>",
                        "Hybrid Vehicles:", HEV, "<br>",
                        "Electric Vehicles:", FEV, "<br>",
                        "Other Fuel Types:", `B20/CNG`)) |> 
    select(garage_state, desc)
  
  inv_state <-
    inv_data |>
    # sel_inv() |> 
      count(garage_state, name = 'vehicle_count') |> 
      left_join(hover_txt, by = 'garage_state')
  
  # load fuel data
  fuel <- readRDS("./appdata/fur_2018_2021.rds")
  
  # extract states
  etx_states <- inv_data |> dplyr::distinct(garage_state)
  # specify chord colors
  group_cols <- RColorBrewer::brewer.pal(n = 11, name = "Spectral")
  
  
  #### render chord ####
  output$inv_chord <- chorddiag::renderChorddiag({
    
    # inv_data |>
    sel_inv() |>
      dplyr::select(vehicle_class, fuel_class) |> 
      dplyr::mutate(across(.cols = everything(), ~ as.factor(.x))) |>
      dplyr::left_join(fuel_desc, by = 'fuel_class') |> 
      with(table(vehicle_class, 
                 class_desc,
                 dnn = list("Body Type", "Powered By"))) |>
      chorddiag::chorddiag(type = "bipartite", 
                           groupnameFontsize = 14,
                           groupnamePadding = 4,
                           groupColors = group_cols,
                           categorynameFontsize = 16,
                           categorynamePadding = 130,
                           showTicks = FALSE)
  })
  
  output$vehicle_count <- shinydashboard::renderValueBox({
    valueBox(value = nrow(sel_inv()),
             subtitle = "FAA On-Road Reportable Vehicles",
             icon = icon("car"),
             width = NULL)
  })
  
  
  
  # helper function to plot value boxes
  render_value_box <- function(group, subtitle, col, icon = icon('car'), 
                               percent = FALSE) {
    renderValueBox({
      valueBox(value = nrow(filter(sel_inv(), class_group == group)),
               subtitle = subtitle,
               icon = icon,
               color = col,
               width = NULL)
    })
  }
  
  output$ev_count <- render_value_box(group = "Electric",
                                      subtitle = "Electric Vehicles",
                                      col = "green",
                                      icon = icon("plug"))
  
  output$alt_count <- render_value_box(group = "Alternative",
                                      subtitle = "Alt. Fuel Vehicles",
                                      col = "orange",
                                      icon = icon("recycle"))
  
  output$petro_count <- render_value_box(group = "Petroleum",
                                      subtitle = "Petroleum Vehicles",
                                      col = "red",
                                      icon = icon("gas-pump"))
  
  
  output$total_gge <- renderValueBox({
    valueBox(value = format(sum(fuel_dat()$units, na.rm = TRUE), 
                            big.mark = ",", 
                            trim = TRUE, 
                            scientific = FALSE,
                            digits = 0),
             subtitle = "Total Purchased GGEs",
             icon = icon('car'),
             width = NULL)
  })
  
  render_fuel_box <- function(group, subtitle, col, icon = icon('car'), ...) {
    renderValueBox({
      valueBox(value = format(sum(filter(fuel_dat(), 
                                         purchase_fuel %in% group)
                                  $units, na.rm = TRUE),
                              big.mark = ",", 
                              trim = TRUE, 
                              scientific = FALSE,
                              ...),
               subtitle = subtitle,
               col = col,
               icon = icon,
               width = NULL)
    })
  }
  
  output$elec_gge <- render_fuel_box(group = c("ELE"),
                                     subtitle = "GGE Electricity",
                                     col = "green",
                                     icon = icon('plug'),
                                     digits = 2)
  output$alt_gge <- render_fuel_box(group = c("E85", "B20", "CNG", "LPG"),
                                    subtitle = "GGE Alternative Fuels",
                                    col = "light-blue",
                                    icon = icon('recycle'),
                                    digits = 0)
  output$petro_gge <- render_fuel_box(group = c("GAS", "DSL"),
                                      subtitle = "GGE Petroleum",
                                      col = "red",
                                      icon = icon('gas-pump'),
                                      digits = 0)

  #### render sunburst ####
  output$fuel_sunburst <- renderPlotly({
    fuel_class_sum <- 
      fuel_dat() |>
      # fuel |>
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
      fuel_dat() |>
      # fuel |>
      filter(date >= as.Date("2020-10-01")) |> 
      select(date, purchase_fuel, units) |>
      left_join(fuel_xwalk, by = 'purchase_fuel') |> 
      group_by(purchase_fuel, fuel_class, fuel_label) |> 
      summarise(units = sum(units, na.rm = TRUE), .groups = 'drop') |> 
      filter(!is.na(purchase_fuel) & purchase_fuel != 'ELE') |> 
      select(fuel_label, fuel_class, units)
    
    fuel_cols <- tibble(
      fuel_label = c("Alternative", "Petroleum", "BioDiesel (B20)",
                     "Compressed Natural Gas", "Diesel", "Ethanol (E85)",
                     "Unleaded Gasoline", "Liquified Petroleum Gas"),
      color = c("#800000", "#3C8DBC", "#4682B4",
                "#5F9EA0", "#cc0000", "#1E90FF",
                "#ff4d4d", "#B0C4DE")
    )
    
    fuel_class_sum |> 
      rbind(fuel_plt) |> 
      left_join(fuel_cols, by = 'fuel_label') |> 
      plotly::plot_ly(
        labels = ~ fuel_label,
        parents = ~ fuel_class,
        values = ~ round(units, 0),
        colors = ~ fuel_label,
        type = 'sunburst'
      ) |> 
      layout(colorway = ~color)
  })
  
  
  
  #### render map ####
  
  
  state_class_count <- reactive({
    sel_state() |>
      count(garage_state, class_desc, name = "units")
  })
  
  output$total_veh <- renderValueBox({
    valueBox(value = nrow(sel_state()),
             subtitle = "Total vehicles in selected States",
             icon = icon('car'),
             width = NULL)
  })
  
  render_state_box <- function(group, subtitle, col, icon = icon('car'), ...) {
    renderValueBox({
      valueBox(value = format(sum(filter(state_class_count(), 
                                         class_desc %in% group)
                                  $units, na.rm = TRUE),
                              big.mark = ",", 
                              trim = TRUE, 
                              scientific = FALSE,
                              ...),
               subtitle = subtitle,
               col = col,
               icon = icon,
               width = NULL)
    })
  }
  
  output$x1 <- render_state_box(group = c("Electric"),
                                     subtitle = "Electric Vehicles",
                                     col = "green",
                                     icon = icon('plug'),
                                     digits = 2)
  output$x2 <- render_state_box(group = c("Ethanol (E85)", "Gas/Elec Hybrid",
                                          "Other Fuel"),
                                    subtitle = "Alternative Fuel Vehicles",
                                    col = "light-blue",
                                    icon = icon('recycle'),
                                    digits = 0)
  output$x3 <- render_state_box(group = c("Gasoline", "Diesel"),
                                      subtitle = "Petroleum Vehicles",
                                      col = "red",
                                      icon = icon('gas-pump'),
                                      digits = 0)
  
  
  # count by state with hover text added
  # inv_state <- ({
  sel_state <- reactive({
    
    fuel_sel <- 
      if(is.null(input$map_fuel_sel)) {
        pull(distinct(fuel_desc, class_desc), class_desc)
      } else {
        input$map_fuel_sel
      }
    
    class_sel <- 
      if(is.null(input$map_type_sel)) {
        pull(distinct(veh_xwalk, vehicle_class))
      } else {
        input$map_type_sel
      }
    
    inv_data |> 
      left_join(fuel_desc, by = 'fuel_class') |> 
      filter(class_desc %in% fuel_sel) |> 
      filter(vehicle_class %in% class_sel)
    
  })
  
  inv_state <- reactive({
    sel_state() |> 
      count(garage_state, name = 'vehicle_count') #|>
      # left_join(hover_txt, by = 'garage_state')
  })
  
  # draw map
  output$vehicle_map <- renderPlotly({
    g <- list(scope = 'usa',
              projection = list(type = 'albers usa'),
              showlakes = FALSE)
    
    plot_geo(inv_state(),
             type = 'choropleth', 
             locationmode = 'USA-states') |> 
      add_trace(z = ~ vehicle_count,
                locations = ~ garage_state,
                color = ~ vehicle_count,
                hovertemplate = 'vehicle count: %{z:.0f}<extra></extra>') |>
      layout(geo = g) |> 
      hide_colorbar() |> 
      onRender("
        function(el) {
          el.on('plotly_click', function(d) {
            console.log('Click: ', d);
          });
        }
      ")
  })
  
  #### factoids ####
  # factoid <- list()
  # factoid$veh_age <- function(.df) {
  # 
  #   cy <- Sys.Date() |> lubridate::year()
  # 
  #   age <- .df |>
  #     pull(model_year) |>
  #     as.numeric(na.rm = TRUE) |>
  #     (\(x) cy - x)() |>
  #     mean() |>
  #     format(digits = 2)
  # 
  #   m = paste("On average, FAA fleet vehicles are", age, "years old.")
  #   list(age = as.numeric(age),
  #        message = m)
  # }
  # 
  # factoid$type_count <- function(.df, ...) {
  #   .df |> 
  #     count(vehicle_type, name = "vehicles", ...)
  # }
  
  
  
}

##### shiny call #####
shinyApp(ui, server)

