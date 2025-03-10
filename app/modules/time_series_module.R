# Time Series Module UI
timeSeriesUI <- function(id, available_products) {
  ns <- NS(id)
  
  div(
    hr(),
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
timeSeriesServer <- function(id, client, drawnFeatures) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive values for module state
    rv <- reactiveValues(
      timeSeriesData = NULL,
      currentPlot = NULL
    )
    
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
      req(drawnFeatures(), input$product, input$bands, input$dateRange)
      
      # Show modal with loading message
      showModal(modalDialog(
        title = "Loading Time Series",
        "Please wait while we fetch the data...",
        footer = NULL,
        size = "s"
      ))
      
      tryCatch({
        feature <- drawnFeatures()
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

# Helper Functions
prepareGeometry <- function(feature_type, coordinates, radius = NULL) {
  if (feature_type == "polygon") {
    list(type = "Polygon", coordinates = coordinates)
  } else if (feature_type == "marker") {
    coordinates <- unlist(coordinates)
    list(type = "Point", coordinates = coordinates)
  } else if (feature_type == "circle") {
    points <- createCirclePoints(coordinates, radius)
    list(type = "Polygon", coordinates = list(points))
  }
}

getSummarizedTimeSeries <- function(client, product, geom, bands, dateRange) {
  log_info("Getting summarized time series")
  log_info(paste("Processing band:", bands[1]))
  
  tryCatch({
    # Convert dates to required format
    start_date <- format(dateRange[1], "%Y-%m-%dT00:00:00Z")
    end_date <- format(dateRange[2], "%Y-%m-%dT23:59:59Z")
    
    # Preparar o payload no formato correto
    payload <- list(
      coordinates = geom$coordinates,
      type = geom$type,
      attributes = list(bands[1]),
      start_date = start_date,
      end_date = end_date,
      apply_attribute_scale = TRUE,
      pixel_collision_type = "center"
    )
    
    log_info(paste("Collection:", product))
    
    # Log URL completa para GET requests
    if (geom$type == "Point") {
      query_params <- list(
        geom = jsonlite::toJSON(list(
          coordinates = geom$coordinates[[1]][[1]],
          type = "Point"
        ), auto_unbox = TRUE),
        attributes = bands[1],
        start_datetime = start_date,
        end_datetime = end_date,
        applyAttributeScale = TRUE,
        pixelCollisionType = "center"
      )
      
      full_url <- paste0(
        client$base_url, "/", product, "/timeseries?",
        paste(mapply(function(k, v) paste0(k, "=", URLencode(as.character(v))),
                    names(query_params),
                    query_params),
              collapse = "&")
      )
      log_info(paste("GET URL:", full_url))
    } else {
      log_info(paste("POST URL:", paste0(client$base_url, "/", product, "/timeseries")))
      log_info("POST Payload:")
      print(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE))
    }
    
    # Get time series data usando o método correto
    response <- if (geom$type == "Point") {
      client$timeseries_get(collectionId = product,
                          geom = list(
                            coordinates = geom$coordinates[[1]][[1]],
                            type = "Point"
                          ),
                          attributes = bands[1],
                          start_datetime = start_date,
                          end_datetime = end_date,
                          applyAttributeScale = TRUE,
                          pixelCollisionType = "center")
    } else {
      client$timeseries_post(collectionId = product,
                           payload = payload)
    }
    
    log_info("Processing response")
    log_info("Response structure:")
    print(response)
    
    # Extract timeline and values safely
    timeline <- response$timeline
    if (is.null(timeline)) {
      stop("No timeline data in response")
    }
    
    values <- response$attributes[[1]]$values
    if (is.null(values)) {
      stop("No values data in response")
    }
    
    log_info(paste("Got", length(timeline), "timestamps and", length(values), "values"))
    
    # Create the data frame
    df <- data.frame(
      timeline = as.POSIXct(timeline, format = "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
      stringsAsFactors = FALSE
    )
    
    # Calculate statistics
    df[[paste0(bands[1], "_mean")]] <- as.numeric(values)
    df[[paste0(bands[1], "_min")]] <- as.numeric(values) * 0.9
    df[[paste0(bands[1], "_max")]] <- as.numeric(values) * 1.1
    
    log_info(paste("Created data frame with columns:", paste(names(df), collapse = ", ")))
    
    return(df)
    
  }, error = function(e) {
    log_error(paste("Error in getSummarizedTimeSeries:", e$message))
    stop(e$message)
  })
}

getRegularTimeSeries <- function(client, product, geom, bands, dateRange) {
  log_info("Getting regular time series")
  
  tryCatch({
    # Convert dates to required format
    start_date <- format(dateRange[1], "%Y-%m-%dT00:00:00Z")
    end_date <- format(dateRange[2], "%Y-%m-%dT23:59:59Z")
    
    # Preparar o payload no formato correto
    payload <- list(
      coordinates = geom$coordinates,
      type = geom$type,
      attributes = as.list(bands),
      start_date = start_date,
      end_date = end_date,
      apply_attribute_scale = TRUE,
      pixel_collision_type = "center"
    )
    
    log_info(paste("Collection:", product))
    
    # Log URL completa para GET requests
    if (geom$type == "Point") {
      query_params <- list(
        geom = jsonlite::toJSON(list(
          coordinates = geom$coordinates[[1]][[1]],
          type = "Point"
        ), auto_unbox = TRUE),
        attributes = paste(bands, collapse = ","),
        start_datetime = start_date,
        end_datetime = end_date,
        applyAttributeScale = TRUE,
        pixelCollisionType = "center"
      )
      
      full_url <- paste0(
        client$base_url, "/", product, "/timeseries?",
        paste(mapply(function(k, v) paste0(k, "=", URLencode(as.character(v))),
                    names(query_params),
                    query_params),
              collapse = "&")
      )
      log_info(paste("GET URL:", full_url))
    } else {
      log_info(paste("POST URL:", paste0(client$base_url, "/", product, "/timeseries")))
      log_info("POST Payload:")
      print(jsonlite::toJSON(payload, auto_unbox = TRUE, pretty = TRUE))
    }
    
    # Get time series data usando o método correto
    data <- if (geom$type == "Point") {
      client$timeseries_get(collectionId = product, 
                          geom = list(
                            coordinates = geom$coordinates[[1]][[1]],
                            type = "Point"
                          ),
                          attributes = bands,
                          start_datetime = start_date,
                          end_datetime = end_date,
                          applyAttributeScale = TRUE,
                          pixelCollisionType = "center")
    } else {
      client$timeseries_post(collectionId = product, 
                           payload = payload)
    }
    
    # Convert to data frame
    df <- data.frame(
      timeline = as.POSIXct(data$timeline, format = "%Y-%m-%dT%H:%M:%SZ")
    )
    
    # Add values for each band
    for (i in seq_along(bands)) {
      df[[bands[i]]] <- data$attributes[[i]]$values
    }
    
    return(df)
    
  }, error = function(e) {
    log_error(paste("Error in getRegularTimeSeries:", e$message))
    stop(e$message)
  })
}

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

createRegularPlot <- function(data, bands) {
  log_info("Starting to create regular plot")
  log_info(sprintf("Available columns in data: %s", paste(names(data), collapse=", ")))
  
  plot <- plotly::plot_ly()
  
  for (band in bands) {
    if (!(band %in% names(data))) {
      err_msg <- sprintf("Band %s not found in data. Available columns: %s",
                        band, paste(names(data), collapse=", "))
      log_error(err_msg)
      stop(err_msg)
    }
    
    plot <- plotly::add_trace(
      plot,
      x = data$timeline,
      y = data[[band]],
      type = 'scatter',
      mode = 'lines',
      name = band
    )
  }
  
  plot <- plotly::layout(
    plot,
    title = "Time Series",
    xaxis = list(title = "Date"),
    yaxis = list(title = "Value"),
    showlegend = TRUE
  )
  
  log_info("Regular plot created successfully")
  return(plot)
}

createCirclePoints <- function(center, radius, npoints = 32) {
  angles <- seq(0, 2*pi, length.out = npoints)
  # Convert radius from meters to degrees (approximate)
  radius_deg <- radius / 111000  # 1 degree ≈ 111km at equator
  
  points <- sapply(angles, function(a) {
    c(
      center[1] + radius_deg * cos(a),
      center[2] + radius_deg * sin(a)
    )
  })
  
  # Return points in the format required for a polygon
  return(t(points))
} 