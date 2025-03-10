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

source("wtss/wtss_client.R")

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

# Initialize STAC client
stac_obj <- stac("https://data.inpe.br/bdc/stac/v1/")

# Get available collections for the dropdown
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
      #plotPanel {
        z-index: 1000;
      }
      .well {
        background-color: white;
        border: none;
        box-shadow: none;
      }
    "))
  ),
  # Sidebar with form
  div(class = "sidebar",
    tabsetPanel(
      tabPanel("Time Series",
        hr(),
        # Product Selection
        selectInput("product", "Select Satellite Product",
                  choices = available_products,
                  selected = NULL),
        # Summarise Geometry Checkbox
        checkboxInput("summariseGeometry", "Summarise Geometry", value = FALSE),
        # Dynamic Bands Selection
        uiOutput("bandSelector"),
        # Date Range Selection
        dateRangeInput("dateRange", "Select Date Range",
                      start = Sys.Date() - 365,
                      end = Sys.Date(),
                      format = "yyyy-mm-dd"),
        # Submit Button
        actionButton("getData", "Get Time Series",
                    class = "btn-primary btn-block",
                    style = "margin-top: 20px;")
      ),
      tabPanel("Image Viewer",
      hr(),
      # Product Selection
      selectInput("collection", "Select Collection",
                 choices = available_collections,
                 selected = NULL),
      # Dynamic Bands Selection
      uiOutput("bandSelectorStac"),
      # Date Range Selection
      dateRangeInput("dateRangeStac", "Select Date Range",
                    start = Sys.Date() - 365,
                    end = Sys.Date(),
                    format = "yyyy-mm-dd"),
      # Search Button
      actionButton("searchAssets", "Search Assets",
                  class = "btn-primary btn-block",
                  style = "margin-top: 20px;"),
      hr(),
      # Assets Selection
      uiOutput("assetSelector"),
      # Load to Map Button
      actionButton("loadToMap", "Load to Map",
                  class = "btn-success btn-block",
                  style = "margin-top: 10px;")
      )
    )

  ),
  # Main panel with map
  div(class = "map-container",
    leafletOutput("map", height = "100%"),
    # Plot panel
    div(
      id = "plotPanel",
      style = "display: none;",  # Initially hidden
      absolutePanel(
        class = "panel panel-default",
        fixed = TRUE,
        draggable = TRUE,
        top = 60,
        right = 20,
        width = "500px",
        height = "400px",
        style = "background-color: white; padding: 15px; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.1);",
        plotlyOutput("timeSeries", width = '100%', height = '350px'),
        actionButton("closePlot", "Close", class = "btn-sm btn-danger")
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  # Log application start
  log_info("WTSS Explorer application started")
  
  # Reactive values to store drawn features and selected data
  rv <- reactiveValues(
    drawnFeatures = NULL,
    timeSeriesData = NULL,
    mapBounds = NULL,
    availableAssets = NULL,
    selectedAsset = NULL,
    overlayGroups = character(0)  # Track overlay groups
  )
  
  # Dynamic band selector based on selected product
  output$bandSelector <- renderUI({
    req(input$product)
    log_info("User selected product: {input$product}")
    collection_info <- client$get_collection_info(input$product)
    band_names <- sapply(collection_info$bands, function(x) x$name)
    
    # Allow single selection if summariseGeometry is checked
    selectInput("bands", "Select Band/Index",
                choices = band_names,
                selected = band_names[1],
                multiple = !input$summariseGeometry)
  })
  
  # Dynamic band selector based on selected product
  output$bandSelectorStac <- renderUI({
    req(input$collection)
    log_info("User selected collection: {input$collection}")
    
    collection_info <- stac_obj %>%
      collections(input$collection) %>%
      get_request()
    
    # Get available bands from the collection's assets
    if (!is.null(collection_info$assets)) {
      band_names <- names(collection_info$assets)
    } else {
      # Fallback to a sample item if collection doesn't specify assets
      sample_item <- stac_obj %>%
        stac_search(
          collections = input$collection,
          limit = 1
        ) %>%
        get_request()
      
      if (length(sample_item$features) > 0) {
        band_names <- names(sample_item$features[[1]]$assets)
      } else {
        band_names <- character(0)
      }
    }
    
    tagList(
      checkboxInput("viewAsRGB", "View as True Color", FALSE),
      conditionalPanel(
        condition = "input.viewAsRGB == false",
        selectInput("bandsStac", "Select Bands",
                   choices = band_names,
                   multiple = TRUE,
                   selected = NULL)
      ),
      conditionalPanel(
        condition = "input.viewAsRGB == true",
        selectInput("redBand", "Red Band",
                   choices = band_names,
                   selected = NULL),
        selectInput("greenBand", "Green Band",
                   choices = band_names,
                   selected = NULL),
        selectInput("blueBand", "Blue Band",
                   choices = band_names,
                   selected = NULL)
      )
    )
  })

  # Initialize the map
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
  
  # Store map bounds when they change
  observeEvent(input$map_bounds, {
    rv$mapBounds <- input$map_bounds
  })
  
  # Handle drawn features
  observeEvent(input$map_draw_new_feature, {
    feature_type <- input$map_draw_new_feature$properties$feature_type
    log_info("User drew a new {feature_type} on the map")
    rv$drawnFeatures <- input$map_draw_new_feature
  })
  
  # Handle deleted features
  observeEvent(input$map_draw_deleted_features, {
    log_info("User deleted drawn features from the map")
    rv$drawnFeatures <- NULL
    hide("plotPanel")
  })
  
  # Get Time Series Data
  observeEvent(input$getData, {
    req(rv$drawnFeatures, input$product, input$bands, input$dateRange)
    log_info("User requested time series data for product: {input$product}")
    
    # Convert the drawn feature to the required format
    feature_type <- rv$drawnFeatures$properties$feature_type
    coordinates <- rv$drawnFeatures$geometry$coordinates
    
    # Prepare geometry based on feature type
    if (feature_type == "polygon") {
      geom <- list(
        type = "Polygon",
        coordinates = coordinates
      )
    } else if (feature_type == "marker") {
      geom <- list(
        type = "Point",
        coordinates = coordinates
      )
    } else if (feature_type == "circle") {
      center <- coordinates
      radius <- rv$drawnFeatures$properties$radius
      # Convert circle to polygon points
      points <- lapply(seq(0, 360, by = 10), function(angle) {
        x <- center[[1]] + radius * cos(angle * pi / 180)
        y <- center[[2]] + radius * sin(angle * pi / 180)
        c(x, y)
      })
      # Close the polygon
      points[[length(points) + 1]] <- points[[1]]
      geom <- list(
        type = "Polygon",
        coordinates = list(points)
      )
    }
    # Get time series data
    tryCatch({
      log_info("Requesting time series data from WTSS service")

      if (input$summariseGeometry) {
        # Use summarize_get for summarized geometry
        summary_data <- client$summarize_get(
          collectionId = input$product,
          geom = geom,
          attributes = input$bands,
          start_datetime = paste0(format(input$dateRange[1], "%Y-%m-%d"), "T00:00:00Z"),
          end_datetime = paste0(format(input$dateRange[2], "%Y-%m-%d"), "T00:00:00Z")
        )
        
        # Access the values dynamically
        band_name <- input$bands  # Get the dynamic band name
        
        df <- parseSummaryDataToDF(summary_data)


        rv$timeSeriesData <- data.frame(
          date = df[['date']], 
          max = df[[paste0(band_name, '_max')]],
          min = df[[paste0(band_name, '_min')]],
          avg = df[[paste0(band_name, '_mean')]]
        )

        plot <- plot_ly(data = rv$timeSeriesData, x = ~date) %>%
          add_trace(y = ~max, name = "Max", type = "scatter", mode = "lines") %>%
          add_trace(y = ~min, name = "Min", type = "scatter", mode = "lines") %>%
          add_trace(y = ~avg, name = "Average", type = "scatter", mode = "lines") %>%
          layout(title = "Summarized Time Series Data",
                xaxis = list(title = "Date"),
                yaxis = list(title = "Values"))

      } else {

        response <- client$timeseries_get(
          collectionId = input$product,
          geom = geom,
          attributes = input$bands,
          start_datetime = paste0(format(input$dateRange[1], "%Y-%m-%d"), "T00:00:00Z"),
          end_datetime = paste0(format(input$dateRange[2], "%Y-%m-%d"), "T00:00:00Z")
        )
        
        # Parse and store the data
        rv$timeSeriesData <- parseGetTimeSeriesToDF(response)

        plot <- plot_ly()
        for(band in input$bands) {
          plot <- plot %>% add_trace(
            data = rv$timeSeriesData,
            x = ~date,
            y = as.formula(paste0("~", band)),
            name = band,
            type = "scatter",
            mode = "lines"
          )
        }

      }

      log_info(paste0("Successfully retrieved and parsed time series data", ", columns:", paste(names(rv$timeSeriesData), collapse=';')))
      
      # Show the plot panel
      shinyjs::show("plotPanel")
      log_info("Displaying time series plot")

      output$timeSeries <- renderPlotly({
        plot
      })

    }, error = function(e) {
      log_error("Error getting time series data: {e$message}")
      showNotification(
        paste("Error getting time series data:", e$message),
        type = "error"
      )
    })
  })
  
  # Close plot panel
  observeEvent(input$closePlot, {
    log_info("User closed the time series plot")
    hide("plotPanel")
  })
  
  
  # Search for assets when button is clicked
  observeEvent(input$searchAssets, {
    req(input$collection, rv$mapBounds, input$dateRangeStac)
    log_info("Searching for assets in collection: {input$collection}")
    
    bbox <- c(
      rv$mapBounds$west,
      rv$mapBounds$south,
      rv$mapBounds$east,
      rv$mapBounds$north
    )
    
    tryCatch({
      # Format datetime string
      datetime <- paste0(
        format(input$dateRangeStac[1], "%Y-%m-%dT00:00:00Z"),
        "/",
        format(input$dateRangeStac[2], "%Y-%m-%dT23:59:59Z")
      )
      
      items <- stac_obj %>%
        stac_search(
          collections = input$collection,
          bbox = bbox,
          datetime = datetime,
          limit = 100
        ) %>%
        get_request()
      
      rv$availableAssets <- items
      
      # Update asset selector
      output$assetSelector <- renderUI({
        if (length(items$features) > 0) {
          choices <- sapply(items$features, function(x) x$properties$datetime)
          names(choices) <- format(as.POSIXct(choices), "%Y-%m-%d %H:%M:%S")
          
          selectInput("selectedAsset", "Select Asset by Date",
                     choices = choices,
                     selected = NULL)
        } else {
          div(
            class = "alert alert-warning",
            "No assets found for the selected criteria"
          )
        }
      })
      
    }, error = function(e) {
      log_error("Error searching assets: {e$message}")
      showNotification(
        paste("Error searching assets:", e$message),
        type = "error"
      )
    })
  })
  
  # Load selected asset to map
  observeEvent(input$loadToMap, {
    req(input$selectedAsset, rv$availableAssets, rv$mapBounds)
    if (!input$viewAsRGB) {
      req(input$bandsStac)
    } else {
      req(input$redBand, input$greenBand, input$blueBand)
    }
    log_info("Loading asset to map")
    
    # Find selected feature
    selected_feature <- Filter(
      function(x) x$properties$datetime == input$selectedAsset,
      rv$availableAssets$features
    )[[1]]
    
    if (!is.null(selected_feature)) {
      tryCatch({
        # Get asset URLs based on view mode
        if (!input$viewAsRGB) {
          asset_urls <- sapply(input$bandsStac, function(band) {
            selected_feature$assets[[band]]$href
          })
        } else {
          rgb_bands <- c(input$redBand, input$greenBand, input$blueBand)
          asset_urls <- sapply(rgb_bands, function(band) {
            selected_feature$assets[[band]]$href
          })
        }
        
        # Create extent in WGS84 (EPSG:4326)
        map_extent <- terra::ext(
          rv$mapBounds$west,
          rv$mapBounds$east,
          rv$mapBounds$south,
          rv$mapBounds$north
        )
        # Convert extent to SpatVector with WGS84 CRS
        map_bbox <- terra::vect(map_extent, crs="EPSG:4326")
        
        # Get date for layer naming
        asset_date <- format(as.POSIXct(selected_feature$properties$datetime), "%Y-%m-%d")
        
        if (!input$viewAsRGB) {
          # Single band processing
          new_layers <- paste(input$collection, names(asset_urls), asset_date, sep = "_")
          
          # Update overlay groups
          rv$overlayGroups <- unique(c(rv$overlayGroups, new_layers))
          
          # First update the layer control with all groups
          leafletProxy("map") %>%
            clearGroup(new_layers) %>%
            addLayersControl(
              baseGroups = c("OpenStreetMap", "Satellite"),
              overlayGroups = rv$overlayGroups,
              options = layersControlOptions(collapsed = FALSE),
              position = "topright"
            )
          
          # Load each band and add to map
          for (band_name in names(asset_urls)) {
            url <- asset_urls[[band_name]]
            
            # Add VSI prefix for cloud-optimized GeoTIFFs
            if (!grepl("^/vsi", url)) {
              url <- paste0("/vsicurl/", url)
            }
            
            # Read raster
            rast <- terra::rast(url)
            
            # Transform extent to raster's CRS and crop
            if (!is.na(terra::crs(rast)) && terra::crs(rast) != "EPSG:4326") {
              map_bbox_transformed <- terra::project(map_bbox, terra::crs(rast))
              rast_cropped <- terra::crop(rast, map_bbox_transformed)
              rast_final <- terra::project(rast_cropped, "EPSG:4326")
            } else {
              rast_final <- terra::crop(rast, map_bbox)
            }
            
            # Create layer name with collection name
            layer_name <- paste(input$collection, band_name, asset_date, sep = "_")
            
            # Add raster to map
            leafletProxy("map") %>%
              addRasterImage(
                rast_final,
                layerId = layer_name,
                group = layer_name,
                opacity = 1,
                project = TRUE,
                method = "bilinear"
              ) %>%
              showGroup(layer_name)
          }
        } else {
          # RGB composite processing
          # Create RGB layer name
          rgb_layer_name <- paste(input$collection, "RGB", asset_date, sep = "_")
          
          # Update overlay groups
          rv$overlayGroups <- unique(c(rv$overlayGroups, rgb_layer_name))
          
          # Update layer control
          leafletProxy("map") %>%
            clearGroup(rgb_layer_name) %>%
            addLayersControl(
              baseGroups = c("OpenStreetMap", "Satellite"),
              overlayGroups = rv$overlayGroups,
              options = layersControlOptions(collapsed = FALSE),
              position = "topright"
            )
          
          # Load and process RGB bands
          rgb_rasts <- list()
          for (i in 1:3) {
            url <- asset_urls[i]
            if (!grepl("^/vsi", url)) {
              url <- paste0("/vsicurl/", url)
            }
            
            rast <- terra::rast(url)
            
            if (!is.na(terra::crs(rast)) && terra::crs(rast) != "EPSG:4326") {
              map_bbox_transformed <- terra::project(map_bbox, terra::crs(rast))
              rast_cropped <- terra::crop(rast, map_bbox_transformed)
              rgb_rasts[[i]] <- terra::project(rast_cropped, "EPSG:4326")
            } else {
              rgb_rasts[[i]] <- terra::crop(rast, map_bbox)
            }
          }
          
          # # Create RGB composite
          # rgb_stack <- terra::rast(rgb_rasts)
          
          # Get values for each band
          r <- rgb_rasts[[1]]
          g <- rgb_rasts[[2]]
          b <- rgb_rasts[[3]]

          # Stack RGB bands into a single raster
          rgb_raster <- c(r, g, b)

          rgb_raster <- brick(rgb_raster) # Convert to RasterBrick for leaflet compatibility
          
          # Add RGB composite to map
          leafletProxy("map") %>%
            addRasterRGB(
              rgb_raster, 
              r = 1, 
              g = 2, 
              b = 3,
              layerId = rgb_layer_name,
              group = rgb_layer_name,
              opacity = 0.9,
            ) %>%
            showGroup(rgb_layer_name)
        }
        
        showNotification("Asset loaded successfully", type = "message")
        
      }, error = function(e) {
        log_error("Error loading asset: {e$message}")
        showNotification(
          paste("Error loading asset:", e$message),
          type = "error"
        )
      })
    }
  })

  # Log when session ends
  session$onSessionEnded(function() {
    log_info("User session ended")
  })
}

# Run the application
shinyApp(ui = ui, server = server) 