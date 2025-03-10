library(shiny)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(jsonlite)
library(httr)
library(httr2)
library(base64enc)
library(R6)
library(shinyjs)
library(shinydashboard)
library(logger)  # Add logging library
library(rstac)
library(terra)
library(sf)
library(leafem)
library(raster)
library(purrr)  # Adicionada esta linha

source("wtss/wtss_client.R")

# Source modules
source("modules/time_series_module.R")
source("modules/image_viewer_module.R")

# Set up logging configuration
log_dir <- "logs"
if (!dir.exists(log_dir)) {
  dir.create(log_dir)
}
log_file <- file.path(log_dir, paste0("wtss_app_", format(Sys.time(), "%Y%m%d"), ".log"))
log_threshold(TRACE)
log_appender(appender_file(log_file))

# Initialize the WTSS client
wtss_inpe <- "https://data.inpe.br/bdc/wtss/v4/"
client <- WTSSClient$new(base_url = wtss_inpe)

# Get available collections for the dropdown
capabilities <- client$get_capabilities()
available_products <- sapply(capabilities$available_collections, function(x) x$name)

# Initialize STAC client and get collections
stac_obj <- stac("https://data.inpe.br/bdc/stac/v1/")
collections <- stac_obj %>%
  collections() %>%
  get_request()

available_collections <- sapply(collections$collections, function(x) x$id)

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .sidebar { 
        height: calc(100vh - 20px);
        overflow-y: auto;
        padding: 15px;
        background-color: #f8f9fa;
        position: fixed;
        width: 300px;
        left: 15px;
        top: 10px;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
      }
      .map-container {
        height: 100vh;
        margin-left: 330px;
        padding: 0;
      }
    "))
  ),
  div(class = "sidebar",
    tabsetPanel(id = "mainTabs",
      tabPanel("Time Series", 
               timeSeriesUI("timeSeries", available_products)),
      tabPanel("Image Viewer", 
               imageViewerUI("imageViewer", available_collections))
    )
  ),
  div(class = "map-container",
    leafletOutput("map", height = "100%")
  )
)

# Server Definition
server <- function(input, output, session) {
  # Log application start
  log_info("WTSS Explorer application started")
  
  # Reactive values for main app state
  rv <- reactiveValues(
    drawnFeatures = NULL,
    mapBounds = NULL
  )
  
  # Initialize map
  output$map <- renderLeaflet({
    log_info("Initializing map view")
    leaflet() %>%
      addTiles() %>%
      addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") %>%
      addLayersControl(
        baseGroups = c("OpenStreetMap", "Satellite"),
        overlayGroups = character(0),
        options = layersControlOptions(collapsed = TRUE)
      ) %>%
      addDrawToolbar(
        targetGroup = 'Features',
        editOptions = editToolbarOptions(),
        polylineOptions = FALSE,
        rectangleOptions = TRUE,
        circleOptions = TRUE,
        markerOptions = TRUE,
        polygonOptions = TRUE
      ) %>%
      setView(lng = -51.9253, lat = -14.2350, zoom = 4)
  })
  
  # Map proxy for updates
  map <- leafletProxy("map")
  
  # Store map bounds when they change
  observeEvent(input$map_bounds, {
    rv$mapBounds <- input$map_bounds
  })
  
  # Handle drawn features
  observeEvent(input$map_draw_new_feature, {
    feature_type <- input$map_draw_new_feature$properties$feature_type
    log_info(sprintf("User drew a new %s on the map", feature_type))
    rv$drawnFeatures <- input$map_draw_new_feature
  })
  
  # Handle deleted features
  observeEvent(input$map_draw_deleted_features, {
    log_info("User deleted drawn features from the map")
    rv$drawnFeatures <- NULL
  })
  
  # Initialize modules
  timeSeriesServer("timeSeries", 
                   client = client,
                   drawnFeatures = reactive({ rv$drawnFeatures }))
  
  imageViewerServer("imageViewer",
                   stac_obj = stac_obj,
                   mapBounds = reactive({ rv$mapBounds }),
                   map = map)
  
  # Log when session ends
  session$onSessionEnded(function() {
    log_info("User session ended")
  })
}

# Run the application
shinyApp(ui = ui, server = server) 