library(pacman)
pacman::p_load(shiny,
               shiny.semantic,
               shinyjs,
               chorddiag,
               plotly,
               DT,
               htmltools)

states <- c(state.abb, "DC", "AE", "GU", "PR", "AS")

ui <- function() {
  shinyUI(semanticPage(
    # tags$head(tags$style(HTML(css)))
    useShinyjs(),
    sidebar(),
    
  ))
}





ui <- semanticPage(
  (title = "Basic dashboard"),
  dashboardSidebar(
    selectInput("states", label = "States",
                choices = states,
                multiple = TRUE),
    actionButton("resetButton", "Clear Filter")
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(chorddiag::chorddiagOutput("inv_chord"), width = 7),
      # box(DT::dataTableOutput("data_summary"), width = 6)
      card("Fleet Composition",
              "This chart displays information for 'on-road reportable' vehicles, 
              a category which includes all non-industrial wheeled vehicles authorized 
              to travel on public roadways. All on-road reportable vehicles have 
              license plates. This category does not include specialty vehicles used in 
              construction (e.g., bucket loaders) or vehicles without an engine 
              (e.g. trailers).")
    ),
    fluidRow(
      box(selectInput("states", label = "States",
                      choices = states,
                      multiple = TRUE),
          actionButton("resetButton", "Clear Filter")),
      valueBox("Title", "x", icon = icon("car"), color = "light-blue")
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
  
   
  
}

shinyApp(ui, server)
