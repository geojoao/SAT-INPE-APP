library(geojsonsf)
library(jsonlite)

# Initialize the WTSS client
wtss_inpe <- "https://data.inpe.br/bdc/wtss/v4/"
client <- WTSSClient$new(base_url = wtss_inpe)

# Get available collections for the dropdown
capabilities <- client$get_capabilities()
available_products <- sapply(capabilities$available_collections, function(x) x$name)

createSummaryPlot <- function(data, band_name) {
  log_info(sprintf("Creating summary plot for band: %s", band_name))
  log_info(sprintf("Data columns: %s", paste(names(data), collapse=", ")))
  
  # Define column names
  mean_col <- paste0(band_name, "_mean")
  min_col <- paste0(band_name, "_min")
  max_col <- paste0(band_name, "_max")
  
  log_info(sprintf("Looking for columns: %s, %s, %s", mean_col, min_col, max_col))
  
  # Check if columns exist
  if (!all(c(mean_col, min_col, max_col) %in% names(data))) {
    err_msg <- sprintf("Missing required columns. Available: %s", paste(names(data), collapse=", "))
    log_error(err_msg)
    stop(err_msg)
  }
  
  log_info("Creating plot object")
  p <- plotly::plot_ly()
  
  log_info("Adding mean trace")
  p <- plotly::add_trace(p,
    x = data$date,
    y = as.numeric(data[[mean_col]]),
    name = "Mean",
    type = "scatter",
    mode = "lines",
    line = list(color = "rgb(0, 100, 0)")
  )
  
  log_info("Adding min trace")
  p <- plotly::add_trace(p,
    x = data$date,
    y = as.numeric(data[[min_col]]),
    name = "Min",
    type = "scatter",
    mode = "lines",
    line = list(color = "rgb(200, 200, 200)")
  )
  
  log_info("Adding max trace")
  p <- plotly::add_trace(p,
    x = data$date,
    y = as.numeric(data[[max_col]]),
    name = "Max",
    type = "scatter",
    mode = "lines",
    line = list(color = "rgb(200, 200, 200)")
  )
  
  log_info("Adding layout")
  p <- plotly::layout(p,
    title = paste("Time Series for", band_name),
    xaxis = list(title = "Date"),
    yaxis = list(title = band_name)
  )
  
  log_info("Plot created successfully")
  return(p)
}

generate_six_digit_number <- function() {

# Generate random digits between 0 and 9
digits <- sample(0:9, 6, replace = TRUE)

# Combine the digits into a single string
number_string <- paste(digits, collapse = "")

# Convert the string to an integer
number <- as.integer(number_string)
return(number)
}

# Function to convert WKT POLYGON to the desired JSON format
wkt_to_json <- function(wkt) {
  # Remove the "POLYGON ((" and "))" parts
  wkt <- gsub("POLYGON \\(\\(|\\)\\)", "", wkt)
  
  # Split the coordinates by comma
  coords <- strsplit(wkt, ",")[[1]]
  
  # Convert the coordinates to a nested list
  coordinates <- lapply(coords, function(coord) {
    as.numeric(strsplit(trimws(coord), " ")[[1]])
  })
  
  # Create the JSON-like list
  json_list <- list(
    properties = list(
      `_leaflet_id` = generate_six_digit_number(),
      feature_type = "polygon"
    ),
    geometry = list(
      type = "Polygon",
      coordinates = list(coordinates)
    )
  )
  
  return(json_list)
}


# Time Series Module UI
timeSeriesUI <- function(id) {
  ns <- NS(id)
  
  div(
    hr(),
    uiOutput(ns('file1_ui')), ## instead of fileInput('file1', label = NULL)
    selectInput(ns("product"), "Select Satellite Product",
                choices = available_products,
                selected = NULL),
    checkboxInput(ns("summariseGeometry"), "Summarise Geometry", value = FALSE),
    uiOutput(ns("bandSelector")),
    dateRangeInput(ns("dateRange"), "Select Date Range",
                   start = Sys.Date() - 365,
                   end = Sys.Date(),
                   format = "yyyy-mm-dd"),
    actionButton(ns("getData"), "Get Time Series",
                 class = "btn-primary btn-block",
                 style = "margin-top: 20px;")
  )
}

# Time Series Module Server
timeSeriesServer <- function(id, leaflet_map = leaflet_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy
    
    # Reactive values for module state
    rv <- reactiveValues(
      timeSeriesData = NULL,
      currentPlot = NULL,
      userGeometry = NULL
    )
    
    # Handle drawn features
    observeEvent(mainInput$map_draw_new_feature, {
      feature_type <- mainInput$map_draw_new_feature$properties$feature_type
      log_info(sprintf("User drew a new %s on the map", feature_type))
      rv$userGeometry <- mainInput$map_draw_new_feature
    })
    
    output$file1_ui <- renderUI({
      fileInput(ns("kmlUpload"), "Upload KML geometry file", accept = ".kml")
    })

    # Handle deleted features
    observeEvent(mainInput$map_draw_deleted_features, {
      log_info("User deleted drawn features from the map")
      rv$userGeometry <- NULL
      map %>%
        clearShapes()
      output$file1_ui <- renderUI({
        fileInput(ns("kmlUpload"), "Upload KML geometry file", accept = ".kml")
      })
    })

    # Handle deleted features
    observeEvent(rv$userGeometry, {
      feature <- rv$userGeometry
      log_info(sprintf("userGeometry updated: %s",toString(feature)))
    })

    # KML File Upload: read and add geometry to map
    observeEvent(input$kmlUpload, {
      kml_path <- input$kmlUpload$datapath
      
      # Read geometry from the KML file using sf with error handling
      geom_sf <- tryCatch({
        st_read(kml_path)
      }, error = function(e) {
        showNotification("Error reading KML file. Please check the file.", type = "error")
        NULL
      })
      
      geom_sf <- st_zm(geom_sf)

      geom_sf <- st_cast(geom_sf, "MULTIPOLYGON") %>% st_cast("POLYGON")


      geom_sf <- st_transform(geom_sf, crs = 4326)

      # Add or update the KML geometry on the leaflet map:
      map %>%
        clearShapes() %>%
        addPolygons(
          data = geom_sf,
          group = "Features",
          #layerId = "Features",
          fillColor = "blue",
          fillOpacity = 0.2,
          color = "blue",
          weight = 2
        ) %>%
        flyToBounds(
          lng1 = st_bbox(geom_sf)$xmin[[1]],
          lat1 = st_bbox(geom_sf)$ymin[[1]],
          lng2 = st_bbox(geom_sf)$xmax[[1]],
          lat2 = st_bbox(geom_sf)$ymax[[1]]
        ) %>%
        showGroup("Features")
      
      showNotification("KML file loaded. You can now edit the geometry on the map.", type = "message")

      # Extract the WKT representation of the geometry
      wkt <- st_as_text(geom_sf$geometry)

      # Convert the WKT to the desired JSON format
      json_geometry <- wkt_to_json(wkt)

      rv$userGeometry <- json_geometry

    })
    
    # Observe draw events to update the map
    # observeEvent(input$map_draw_new_feature, {
    #   feature <- input$map_draw_new_feature
    #   map %>%
    #     addPolygons(
    #       data = feature,
    #       group = "Features",
    #       fillColor = "blue",
    #       fillOpacity = 0.2,
    #       color = "blue",
    #       weight = 2
    #     )
    # })
    
    # observeEvent(input$map_draw_edited_features, {
    #   edited_features <- input$map_draw_edited_features
    #   map %>%
    #     clearGroup("Features") %>%
    #     addPolygons(
    #       data = edited_features,
    #       group = "Features",
    #       fillColor = "blue",
    #       fillOpacity = 0.2,
    #       color = "blue",
    #       weight = 2
    #     )
    # })
    
    # observeEvent(input$map_draw_deleted_features, {
    #   deleted_features <- input$map_draw_deleted_features
    #   map %>%
    #     clearGroup("Features")
    # })
    
    # Dynamic band selector based on selected product
    output$bandSelector <- renderUI({
      req(input$product)
      collection_info <- client$get_collection_info(input$product)
      band_names <- sapply(collection_info$bands, function(x) x$name)
      
      selectInput(ns("bands"), "Select Band/Index",
                  choices = band_names,
                  selected = band_names[1],
                  multiple = !input$summariseGeometry)
    })
    
    # Get Time Series Data
    observeEvent(input$getData, {
      req(rv$userGeometry, input$product, input$bands, input$dateRange)
      
      # Show modal with loading message
      showModal(modalDialog(
        title = "Loading Time Series",
        "Please wait while we fetch the data...",
        footer = NULL,
        size = "s"
      ))
      
      tryCatch({
        feature <- rv$userGeometry
        feature_type <- feature$properties$feature_type
        coordinates <- feature$geometry$coordinates
        
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
          center <- unlist(coordinates)
          radius <- feature$properties$radius
          
          # Create a point using sf
          point <- st_point(c(center[1], center[2]))  # Longitude, Latitude
          sf_point <- st_sfc(point, crs = 4326)  # Define the coordinate reference system (WGS84)
          
          # Create a buffer around the point to form a circle
          geom <- st_buffer(sf_point, dist = radius / 111000)  # Convert radius from meters to degrees
          
          # Convert to GeoJSON format for the API
          geom <- st_as_text(geom)  # Convert to WKT format
          geom <- list(type = "Polygon", coordinates = list(geom))
        }

        if (input$summariseGeometry) {
          # Use summarize_get for summarized geometry
          response <- client$summarize_get(
            collectionId = input$product,
            geom = geom,
            attributes = input$bands,
            start_datetime = paste0(format(input$dateRange[1], "%Y-%m-%d"), "T00:00:00Z"),
            end_datetime = paste0(format(input$dateRange[2], "%Y-%m-%d"), "T00:00:00Z")
          )
          
          log_info("Processing summarized response")
          
          # Use the parseSummaryDataToDF function from wtss_client.R
          df <- parseSummaryDataToDF(response)
          
          log_info(sprintf("Created summarized dataframe with columns: %s", 
                          paste(names(df), collapse=", ")))
          
          # Create summary plot using the helper function
          p <- createSummaryPlot(df, input$bands[1])
          
        } else {
          # Use regular time series get
          response <- client$timeseries_get(
            collectionId = input$product,
            geom = geom,
            attributes = input$bands,
            start_datetime = paste0(format(input$dateRange[1], "%Y-%m-%d"), "T00:00:00Z"),
            end_datetime = paste0(format(input$dateRange[2], "%Y-%m-%d"), "T00:00:00Z")
          )
          
          log_info("Processing time series response")
          
          # Verificar se temos dados na resposta
          if (is.null(response$results) || length(response$results) == 0) {
            stop("No data available in the response")
          }
          
          # Extrair os dados da série temporal do primeiro resultado
          time_series <- response$results[[1]]$time_series
          
          if (is.null(time_series$timeline) || length(time_series$timeline) == 0) {
            stop("No timeline data available")
          }
          
          # Create data frame with dates in correct format
          df <- data.frame(
            date = as.Date(unlist(time_series$timeline)),
            stringsAsFactors = FALSE
          )
          
          # Add values for each band
          for (band in input$bands) {
            if (!is.null(time_series$values[[band]])) {
              df[[band]] <- unlist(time_series$values[[band]])
              log_info(sprintf("Added band %s to dataframe", band))
            } else {
              log_error(sprintf("Could not find values for band %s", band))
              log_info("Available bands in response:")
              print(names(time_series$values))
              stop(paste("Could not find values for band", band))
            }
          }
          
          log_info(sprintf("Created dataframe with columns: %s", paste(names(df), collapse=", ")))
          
          # Create plot using direct column references
          p <- plot_ly(data = df, x = ~date) %>%
            add_trace(
              y = df[[input$bands[1]]],
              name = input$bands[1],
              type = "scatter",
              mode = "lines"
            )
          
          # Add additional bands if multiple selected
          if (length(input$bands) > 1) {
            for (i in 2:length(input$bands)) {
              p <- add_trace(
                p,
                y = df[[input$bands[i]]],
                name = input$bands[i],
                type = "scatter",
                mode = "lines"
              )
            }
          }
        }
        
        # Update layout
        p <- layout(p,
          title = paste("Time Series for", paste(input$bands, collapse=", ")),
          xaxis = list(title = "Date"),
          yaxis = list(title = "Value"),
          showlegend = TRUE
        )
        
        rv$currentPlot <- p
        
        # Remove loading modal and show plot
        removeModal()
        showModal(modalDialog(
          title = "Time Series Plot",
          plotlyOutput(ns("modalPlot"), height = "400px"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
        
      }, error = function(e) {
        removeModal()
        showNotification(
          paste("Error getting time series data:", e$message),
          type = "error"
        )
      })
    })
    
    # Render plot in modal
    output$modalPlot <- renderPlotly({
      req(rv$currentPlot)
      rv$currentPlot
    })
    
  })
}
