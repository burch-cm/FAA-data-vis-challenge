library(pacman)
pacman::p_load(dplyr,
               tidyr,
               shiny,
               shinydashboard,
               shinythemes,
               chorddiag,
               plotly,
               shinyjs,
               htmltools)
# values
state_abb <- c(state.abb, "DC", "AE", "GU", "PR", "AS")
state_name <- c(state.name, 
                "District of Columbia",
                "Armed Forces Europe",
                "Guam",
                "Puerto Rico",
                "American Samoa")
states <- data.frame(garage_state = state_abb, state_name = state_name)

# dashboard
ui <- dashboardPage(
  
  dashboardHeader(title = "FAA Fleet"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Fleet Composition",
               tabName = 'composition',
               icon = icon('car')),
      menuItem("Vehicle Locations",
               tabName = 'vehicleMap',
               icon = icon('map'))
    )
  ),
  
  dashboardBody(
    
    tabItems(
      # first tab
      tabItem(tabName = 'composition',
        fluidRow(
          box(chorddiag::chorddiagOutput("inv_chord",
                                         width = "100%",
                                         height = "500px"), 
              width = 6),
          column(width = 6,
                 valueBoxOutput("vehicle_count", width = NULL),
                 fluidRow(
                   valueBoxOutput("ev_count", width = 4),
                   valueBoxOutput("alt_count", width = 4),
                   valueBoxOutput("petro_count", width = 4)
                 ),
                 fluidRow(
                   box(width = 12,
                       selectInput("states", label = "States",
                                   choices = state_abb,
                                   multiple = TRUE),
                       actionButton("resetButton", "Clear Filter"))
                 ),
                 box(
                   tagList(
                     div("This chart displays information for 'on-road reportable' 
              vehicles, a category which includes all non-industrial wheeled 
              vehicles authorized to travel on public roadways."),
                     tags$br(),
                     div("This category does not include specialty vehicles used in 
              construction (e.g., bucket loaders) or vehicles without an engine 
              (e.g. trailers)."),
                   ),
                   title = "What types of vehicles are tracked?",
                   width = NULL),
          )
        )
      ),
      # second tab
      tabItem(tabName = 'vehicleMap',
        fluidRow(
          box(plotlyOutput("fleet_map"), width = 9),
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # load inventory data
  inv_data <- readr::read_rds("../data/fleet_inv.rds")
  # load fuel data
  fuel <- readr::read_rds("../data/fuel_by_state.rds")
  # extract states
  states <- inv_data |> dplyr::distinct(garage_state)
  # specify chord colors
  group_cols <- RColorBrewer::brewer.pal(n = 11, name = "Spectral")
  # reactive data filter
  sel_inv <- reactive({
    if (is.null(input$states)) {
      inv_data
    } else {
      filter(inv_data, garage_state %in% input$states)
    }
  })
  
  # reset UI on button press
  observeEvent(input$resetButton, {
    updateSelectInput(session, "states", selected = "NULL")
  })
  
  # plot chord
  output$inv_chord <- chorddiag::renderChorddiag({
    sel_inv() |> 
      dplyr::select(vehicle_class, fuel_class) |> 
      dplyr::mutate(across(.cols = everything(), ~ as.factor(.x))) |> 
      with(table(vehicle_class, fuel_class,
                 dnn = list("Vehicle Type", "Fuel Type"))) |>
      chorddiag::chorddiag(type = "bipartite", 
                           groupnameFontsize = 14,
                           groupnamePadding = 4,
                           groupColors = group_cols,
                           categorynameFontsize = 14,
                           categorynamePadding = 50,
                           showTicks = FALSE)
  })
  
  output$vehicle_count <- shinydashboard::renderValueBox({
    valueBox(value = nrow(sel_inv()),
             subtitle = "FAA On-Road Reportable Vehicles",
             icon = icon("car"),
             width = NULL)
  })
   
  # helper function to plot value boxes
  render_value_box <- function(group, subtitle, col, icon = icon('car')) {
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
                                      col = "maroon",
                                      icon = icon("gas-pump"))
  
  # plotly map options
  g <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE,
      lakecolor = toRGB('white')
    )
  
  # plot vehicle map
  us_states <- geojsonio::geojson_read("../json/gz_2010_us_040_00_5m.json", what = "sp")

  output$fleet_map <- renderPlotly({
    plot_geo({inv_data |>
              select(state = garage_state) |>
              count(state, name = "vehicles", sort = TRUE)},
              locationmode = "USA-states") |>
    add_trace(z = ~ vehicles,
              locations = ~ state) |>
    layout(geo = g,
           title = paste(input$fuel_type, "Vehicle Count by State, FY2020"))
  })
  
  # plot fuel timeline
  # output$fuel_timeline <- renderDygraph({
  #   fuel_mx <-
  #     fuel |> 
  #     select(date = purchase_date, purchase_fuel, units) |>
  #     group_by(date, purchase_fuel) |> 
  #     summarize(units = sum(units), .groups = 'drop') |> 
  #     pivot_wider(id_cols = date, names_from = purchase_fuel, values_from = units) |> 
  #     select(date, E85, GAS, DSL)
  #   
  #   fuel_matrix <-
  #     fuel_mx |> 
  #     select(E85, GAS, DSL) |> 
  #     xts(order.by = fuel_mx$date)
  #   
  #   dygraph(fuel_matrix) |> 
  #     dySeries('GAS') |> 
  #     dySeries('E85') |> 
  #     dySeries('DSL') |> 
  #     dyOptions(colors = RColorBrewer::brewer.pal(3, 'Set2'),
  #               stepPlot = TRUE,
  #               fillGraph = TRUE,
  #               fillAlpha = 0.5,
  #               drawGrid = FALSE) |> 
  #     dyAxis('y', label = "Units (GGE)") |> 
  #     dyRangeSelector(height = 20)
  # })
  
}
shinyApp(ui, server)
