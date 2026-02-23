# Startup Modal UI
startupModalUI <- function(id) {
  ns <- NS(id)
  
    modalDialog(
        title = "Welcome to SAT - Sistema de Analise de Terras",
        easyClose = TRUE,
        footer = NULL,
        size = "m",
        div(
            p("Welcome to SAT!"),
            p("This application allows the visualization and analysis of geospatial time series data."),
            p("The main capabilities include:"),
            tags$ul(
            tags$li("Time Series Visualization: Generates interactive time series charts for spectral indices including NDVI and EVI."),
            tags$li("Satellite Imagery Visualization: Provides visualization of satellite imagery from different sources.")
            ),
            p("Explore the data and enjoy the analysis! The goal of this app is to be the ultimate tool for express agricultural land analysis.")
        )
    )
}

# Startup Modal Server
startupModalServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    showModal(startupModalUI(id))
  })
} 