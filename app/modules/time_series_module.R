library(geojsonsf)
library(jsonlite)

# Initialize the WTSS client
wtss_inpe <- "https://data.inpe.br/bdc/wtss/v4/"
client <- WTSSClient$new(base_url = wtss_inpe)

# Get available collections for the dropdown
capabilities <- client$get_capabilities()
available_products <- sapply(capabilities$available_collections, function(x) x$name)

# Default product: MOIDS (mod13q1-6.1 - MODIS Terra Vegetation Indices)
default_product <- if ("mod13q1-6.1" %in% available_products) "mod13q1-6.1" else available_products[1]

# Summary stats: prefer p25/p75 (quantis), fallback to min/max
get_summary_stat_cols <- function(data, band_name) {
  mean_col <- paste0(band_name, "_mean")
  p25_col <- paste0(band_name, "_p25")
  p75_col <- paste0(band_name, "_p75")
  min_col <- paste0(band_name, "_min")
  max_col <- paste0(band_name, "_max")
  
  if (mean_col %in% names(data) && p25_col %in% names(data) && p75_col %in% names(data)) {
    list(mean = mean_col, lower = p25_col, upper = p75_col, lower_label = "P25", upper_label = "P75")
  } else if (mean_col %in% names(data) && min_col %in% names(data) && max_col %in% names(data)) {
    list(mean = mean_col, lower = min_col, upper = max_col, lower_label = "Min", upper_label = "Max")
  } else {
    NULL
  }
}

createSummaryPlot <- function(data, band_name) {
  log_info(sprintf("Creating summary plot for band: %s", band_name))
  log_info(sprintf("Data columns: %s", paste(names(data), collapse=", ")))
  
  cols <- get_summary_stat_cols(data, band_name)
  if (is.null(cols)) {
    err_msg <- sprintf("Missing required columns (mean + p25/p75 or min/max). Available: %s",
                      paste(names(data), collapse=", "))
    log_error(err_msg)
    stop(err_msg)
  }
  
  mean_vals <- as.numeric(data[[cols$mean]])
  lower_vals <- as.numeric(data[[cols$lower]])
  upper_vals <- as.numeric(data[[cols$upper]])
  
  # Plot com rea preenchida: ribbon entre lower e upper, linha da mdia
  p <- plotly::plot_ly(data, x = ~date)
  
  # Trace 1: limite inferior (base para o fill)
  p <- plotly::add_trace(p,
    x = data$date,
    y = lower_vals,
    type = "scatter",
    mode = "lines",
    line = list(width = 0, color = "rgba(0,0,0,0)"),
    showlegend = FALSE
  )
  # Trace 2: limite superior com fill at o trace anterior (ribbon)
  p <- plotly::add_trace(p,
    x = data$date,
    y = upper_vals,
    type = "scatter",
    mode = "lines",
    line = list(width = 0, color = "rgba(0,0,0,0)"),
    fill = "tonexty",
    fillcolor = "rgba(34, 139, 34, 0.25)",
    name = paste0(cols$lower_label, "-", cols$upper_label)
  )
  
  # Linha da mdia
  p <- plotly::add_trace(p,
    x = data$date,
    y = mean_vals,
    type = "scatter",
    mode = "lines",
    line = list(color = "rgb(0, 100, 0)", width = 2.5),
    name = "Mean"
  )
  
  p <- plotly::layout(p,
    title = list(text = paste("Time Series for", band_name), font = list(size = 16)),
    xaxis = list(title = "Date", gridcolor = "rgba(200,200,200,0.5)"),
    yaxis = list(title = band_name, range = c(0, 1), gridcolor = "rgba(200,200,200,0.5)"),
    plot_bgcolor = "rgba(250,250,250,0.8)",
    paper_bgcolor = "white",
    showlegend = TRUE,
    legend = list(orientation = "h", x = 0, y = 1.1)
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
                selected = default_product),
    checkboxInput(ns("summariseGeometry"), "Summarise Geometry", value = FALSE),
    uiOutput(ns("bandSelector")),
    dateRangeInput(ns("dateRange"), "Select Date Range",
                   start = Sys.Date() - 365 * 10,
                   end = Sys.Date(),
                   format = "yyyy-mm-dd"),
    checkboxInput(
      ns("applyQualityFilter"),
      "Filter cloudy / low-quality days",
      value = TRUE
    ),
    selectInput(
      ns("smoothing"),
      "Time series smoothing",
      choices = c(
        "None" = "none",
        "Savitzky-Golay - light" = "sg_light",
        "Savitzky-Golay - medium" = "sg_medium",
        "Savitzky-Golay - strong" = "sg_strong"
      ),
      selected = "none"
    ),
    actionButton(ns("getData"), "Get Time Series",
                 class = "btn-primary btn-block",
                 style = "margin-top: 20px;")
  )
}

# Helper to apply simple quality filter based on collection band metadata
apply_quality_filter <- function(df, bands, collection_info, summarised = FALSE) {
  if (is.null(collection_info) || is.null(collection_info$bands)) {
    return(df)
  }
  
  for (band in bands) {
    meta <- NULL
    for (b in collection_info$bands) {
      if (!is.null(b$name) && b$name == band) {
        meta <- b
        break
      }
    }
    if (is.null(meta)) next
    
    vmin <- NA
    vmax <- NA
    if (!is.null(meta$valid_range) && length(meta$valid_range) == 2) {
      vmin <- meta$valid_range[1]
      vmax <- meta$valid_range[2]
    }
    nodata <- if (!is.null(meta$nodata)) meta$nodata else NA
    
    if (!summarised) {
      # Regular time series: one column per band
      if (!band %in% names(df)) next
      col_vals <- df[[band]]
      if (!is.na(nodata)) {
        col_vals[col_vals == nodata] <- NA
      }
      if (!is.na(vmin) && !is.na(vmax)) {
        col_vals[col_vals < vmin | col_vals > vmax] <- NA
      }
      df[[band]] <- col_vals
    } else {
      # Summarised geometry: multiple stats per band (mean, min, max, p25, p75, etc.)
      stat_suffixes <- c("_mean", "_min", "_max", "_p25", "_p75")
      for (suf in stat_suffixes) {
        col_name <- paste0(band, suf)
        if (!col_name %in% names(df)) next
        col_vals <- df[[col_name]]
        if (!is.na(nodata)) {
          col_vals[col_vals == nodata] <- NA
        }
        if (!is.na(vmin) && !is.na(vmax)) {
          col_vals[col_vals < vmin | col_vals > vmax] <- NA
        }
        df[[col_name]] <- col_vals
      }
    }
  }
  
  df
}

# Helper to clip all band values to [-1, 1]
clip_to_unit_range <- function(df, bands, summarised = FALSE) {
  if (!summarised) {
    for (band in bands) {
      if (!band %in% names(df)) next
      vals <- df[[band]]
      vals <- pmin(pmax(vals, -1), 1)
      df[[band]] <- vals
    }
  } else {
    stat_suffixes <- c("_mean", "_min", "_max", "_p25", "_p75")
    for (band in bands) {
      for (suf in stat_suffixes) {
        col_name <- paste0(band, suf)
        if (!col_name %in% names(df)) next
        vals <- df[[col_name]]
        vals <- pmin(pmax(vals, -1), 1)
        df[[col_name]] <- vals
      }
    }
  }
  df
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
      
      # Default band: EVI if available, otherwise first band
      default_band <- if ("EVI" %in% band_names) "EVI" else band_names[1]
      
      selectInput(ns("bands"), "Select Band/Index",
                  choices = band_names,
                  selected = default_band,
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

        # Get collection info for quality filtering
        collection_info <- client$get_collection_info(input$product)

        if (input$summariseGeometry) {
          # Use summarize_get: prefer mean + p25/p75 (quantis), fallback para mean/min/max
          response <- tryCatch({
            client$summarize_get(
              collectionId = input$product,
              geom = geom,
              attributes = input$bands,
              start_datetime = paste0(format(input$dateRange[1], "%Y-%m-%d"), "T00:00:00Z"),
              end_datetime = paste0(format(input$dateRange[2], "%Y-%m-%d"), "T00:00:00Z"),
              aggregations = c("mean", "p25", "p75")
            )
          }, error = function(e) {
            log_info(sprintf("p25/p75 not available, using default aggregations: %s", e$message))
            client$summarize_get(
              collectionId = input$product,
              geom = geom,
              attributes = input$bands,
              start_datetime = paste0(format(input$dateRange[1], "%Y-%m-%d"), "T00:00:00Z"),
              end_datetime = paste0(format(input$dateRange[2], "%Y-%m-%d"), "T00:00:00Z")
            )
          })
          
          log_info("Processing summarized response")
          
          # Use the parseSummaryDataToDF function from wtss_client.R
          df <- parseSummaryDataToDF(response)
          
          log_info(sprintf("Created summarized dataframe with columns: %s", 
                          paste(names(df), collapse=", ")))
          
          # Apply simple quality filter if requested
          if (isTRUE(input$applyQualityFilter)) {
            df <- apply_quality_filter(df, input$bands, collection_info, summarised = TRUE)
          }
          
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
          
          # Extrair os dados da serie temporal do primeiro resultado
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
          
          # Apply simple quality filter if requested
          if (isTRUE(input$applyQualityFilter)) {
            df <- apply_quality_filter(df, input$bands, collection_info, summarised = FALSE)
          }
        }

        # Optional Savitzky-Golay smoothing on the time series (raw or summarised)
        if (!is.null(input$smoothing) && input$smoothing != "none") {
          window <- switch(
            input$smoothing,
            sg_light = 5,
            sg_medium = 9,
            sg_strong = 15,
            5
          )
          if (window %% 2 == 0) {
            window <- window + 1
          }
          
          if (input$summariseGeometry) {
            # Smooth summary statistics (mean, min, max, p25, p75) for each band
            stat_suffixes <- c("_mean", "_min", "_max", "_p25", "_p75")
            for (band in input$bands) {
              for (suf in stat_suffixes) {
                col_name <- paste0(band, suf)
                if (!col_name %in% names(df)) next
                ts_vals <- df[[col_name]]
                if (all(is.na(ts_vals))) next
                
                ts_clean <- ts_vals
                na_idx <- is.na(ts_clean)
                if (all(na_idx)) next
                ts_clean[na_idx] <- mean(ts_clean[!na_idx], na.rm = TRUE)
                
                if (length(ts_clean) >= window) {
                  smoothed <- signal::sgolayfilt(ts_clean, p = 3, n = window)
                  smoothed[na_idx] <- NA
                  df[[col_name]] <- smoothed
                }
              }
            }
          } else {
            # Smooth raw time series per band
            for (band in input$bands) {
              if (!band %in% names(df)) next
              ts_vals <- df[[band]]
              if (all(is.na(ts_vals))) next
              
              ts_clean <- ts_vals
              na_idx <- is.na(ts_clean)
              if (all(na_idx)) next
              ts_clean[na_idx] <- mean(ts_clean[!na_idx], na.rm = TRUE)
              
              if (length(ts_clean) >= window) {
                smoothed <- signal::sgolayfilt(ts_clean, p = 3, n = window)
                smoothed[na_idx] <- NA
                df[[band]] <- smoothed
              }
            }
          }
        }

        # Clip valores para o intervalo [-1, 1]
        df <- clip_to_unit_range(df, input$bands, summarised = input$summariseGeometry)

        # Create plot
        if (input$summariseGeometry) {
          # Create summary plot using the helper function
          p <- createSummaryPlot(df, input$bands[1])
        } else {
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
