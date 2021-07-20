library(pacman)
pacman::p_load(dplyr,
               tidyr,
               shiny,
               shinydashboard,
               shinythemes,
               chorddiag,
               plotly,
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
    ),
    selectInput("states", label = "States",
                choices = state_abb,
                multiple = TRUE),
    actionButton("resetButton", "Clear Filter")
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
                 box(
                   tagList(
                     div("This chart displays information for 'on-road reportable' 
              vehicles, a category which includes all non-industrial wheeled 
              vehicles authorized to travel on public roadways. 
              All on-road reportable vehicles have license plates."),
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
          plotlyOutput('fleet_map')
        )
      )
    )
  )
)

server <- function(input, output, session) {
  # load inventory data
  inv_data <- readr::read_rds("../data/fleet_inv.rds")
  
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
   
  # output$inv_plot <- renderPlot({
  #   sel_inv() |> 
  #     dplyr::select(garage_state, vehicle_class, fuel_class) |> 
  #     dplyr::mutate(across(everything(), ~ as.factor(.x))) |> 
  #     dplyr::group_by(garage_state) |> 
  #     dplyr::add_count(name = "state_total") |>
  #     dplyr::arrange(desc(state_total)) |> 
  #     dplyr::count(state_total, vehicle_class, 
  #                  fuel_class, name = "vehicles", sort = TRUE) |> 
  #     ggplot2::ggplot(aes(x = reorder(garage_state, state_total), 
  #                         y = vehicles, fill = fuel_class)) +
  #     ggplot2::geom_bar(stat = "identity", position = "stack") +
  #     labs(x = "State", y = "Count of Vehicles", 
  #          title = "Count of Vehicle by State") +
  #     coord_flip()
  # })
  
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
                                      subtitle = "Electric",
                                      col = "green",
                                      icon = icon("plug"))
  
  output$alt_count <- render_value_box(group = "Alternative",
                                      subtitle = "Alt. Fuel",
                                      col = "orange",
                                      icon = icon("recycle"))
  
  output$petro_count <- render_value_box(group = "Petroleum",
                                      subtitle = "Petroleum",
                                      col = "maroon",
                                      icon = icon("gas-pump"))
  
  
  # plot map
  output$fleet_map <- renderPlotly({
    
    hover_text <- 
      inv_data |>
      # sel_inv() |>
      count(garage_state, fuel_class) |> 
      left_join(states, by = 'garage_state') |> 
      pivot_wider(id_cols = c(garage_state, state_name), 
                  names_from = fuel_class, 
                  values_from = n) |>
      replace_na(list(E85 = 0, GAS = 0, HEV = 0, FEV = 0, `B20/CNG` = 0)) |> 
      mutate(hover = paste(state_name, "\n",
                           "E85: ", E85, "\n",
                           "DSL: ", DSL, "\n",
                           "GAS: ", GAS, "\n",
                           "HEV: ", HEV, "\n",
                           "FEV: ", FEV, "\n",
                           "B20/CNG: ", `B20/CNG`, sep = ""))
    f <- 
      inv_data |>
      # sel_inv() |>
      count(garage_state, name = 'total') |>
      left_join(hover_text, by = 'garage_state') |> 
      plot_geo(locationmode = 'USA-states') |> 
      add_trace(locations = ~ garage_state, 
                z = ~total,
                text = ~hover,
                color = ~total,
                colors = "Blues") |> 
      colorbar(title = "Vehicles") |> 
      layout(title = 'FAA Vehicle Count by Garaged State',
             geo = list(scope = 'usa',
                        projection = list(type = 'albers usa'),
                        showlakes = FALSE,
                        lakecolor = toRGB('white')))
    f
  })
  
}
shinyApp(ui, server)
