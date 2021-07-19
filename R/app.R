library(pacman)
pacman::p_load(shiny,
               shinydashboard,
               chorddiag,
               plotly,
               htmltools)

states <- c(state.abb, "DC", "AE", "GU", "PR", "AS")

ui <- dashboardPage(
  
  dashboardHeader(title = "FAA Fleet"),
  
  dashboardSidebar(
    selectInput("states", label = "States",
                choices = states,
                multiple = TRUE),
    actionButton("resetButton", "Clear Filter")
  ),
  
  dashboardBody(
    # useShinyjs(),
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(chorddiag::chorddiagOutput("inv_chord",
                                     width = "100%",
                                     height = "500px"), 
          width = 6),
      column(width = 5,
        # infoBox(title = "Total FAA On-Road Reportable Vehicles",
        #         icon = icon("car"),
        #         value = h2("4559"),
        #         width = NULL),
        # shinydashboard::infoBoxOutput("vehicle_count", width = NULL),
        valueBoxOutput("vehicle_count", width = NULL),
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
        box())
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
  
  # output$vehicle_count <- shinydashboard::renderInfoBox({
  #   infoBox(title = "Vehicle Count",
  #           subtitle = "On-Road Reportable Vehicles",
  #           value = nrow(sel_inv()), 
  #           icon = shiny::icon("car"),
  #           width = NULL)
  # })
  
  output$vehicle_count <- shinydashboard::renderValueBox({
    valueBox(value = nrow(sel_inv()),
             subtitle = "FAA On-Road Reportable Vehicles",
             icon = icon("car"),
             width = NULL)
  })
   
  output$inv_plot <- renderPlot({
    sel_inv() |> 
      dplyr::select(garage_state, vehicle_class, fuel_class) |> 
      dplyr::mutate(across(everything(), ~ as.factor(.x))) |> 
      dplyr::group_by(garage_state) |> 
      dplyr::add_count(name = "state_total") |>
      dplyr::arrange(desc(state_total)) |> 
      dplyr::count(state_total, vehicle_class, 
                   fuel_class, name = "vehicles", sort = TRUE) |> 
      ggplot2::ggplot(aes(x = reorder(garage_state, state_total), 
                          y = vehicles, fill = fuel_class)) +
      ggplot2::geom_bar(stat = "identity", position = "stack") +
      labs(x = "State", y = "Count of Vehicles", 
           title = "Count of Vehicle by State") +
      coord_flip()
  })
  
  
}

shinyApp(ui, server)
