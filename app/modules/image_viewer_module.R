# Image Viewer Module UI
imageViewerUI <- function(id, available_collections) {
  ns <- NS(id)
  
  div(
    hr(),
    selectInput(ns("collection"), "Select Collection",
               choices = available_collections,
               selected = NULL),
    # Dynamic band selector with RGB option
    uiOutput(ns("bandSelectorStac")),
    dateRangeInput(ns("dateRangeStac"), "Select Date Range",
                  start = Sys.Date() - 365,
                  end = Sys.Date(),
                  format = "yyyy-mm-dd"),
    actionButton(ns("searchAssets"), "Search Assets",
                class = "btn-primary btn-block",
                style = "margin-top: 20px;"),
    hr(),
    uiOutput(ns("assetSelector")),
    actionButton(ns("loadToMap"), "Load to Map",
                class = "btn-success btn-block",
                style = "margin-top: 10px;")
  )
}

# Image Viewer Module Server
imageViewerServer <- function(id, stac_obj, mapBounds, map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    rv <- reactiveValues(
      availableAssets = NULL,
      overlayGroups = character(0)
    )
    
    # Dynamic band selector based on selected collection
    output$bandSelectorStac <- renderUI({
      req(input$collection)
      log_info(sprintf("User selected collection: %s", input$collection))
      
      collection <- stac_obj %>%
        collections() %>%
        get_request() %>%
        pluck("collections") %>%
        keep(~.$id == input$collection) %>%
        pluck(1)
      
      # Get available bands
      if (!is.null(collection$summaries$`eo:bands`)) {
        band_names <- sapply(collection$summaries$`eo:bands`, function(x) x$name)
      } else {
        # Fallback to a sample item if collection doesn't specify bands
        sample_item <- stac_obj %>%
          stac_search(collections = input$collection, limit = 1) %>%
          get_request()
        
        if (length(sample_item$features) > 0) {
          band_names <- names(sample_item$features[[1]]$assets)
        } else {
          band_names <- character(0)
        }
      }
      
      tagList(
        checkboxInput(ns("viewAsRGB"), "View as RGB", FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("viewAsRGB")),
          selectInput(ns("bandsStac"), "Select Bands",
                     choices = band_names,
                     multiple = TRUE,
                     selected = NULL)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("viewAsRGB")),
          selectInput(ns("redBand"), "Red Band",
                     choices = band_names,
                     selected = NULL),
          selectInput(ns("greenBand"), "Green Band",
                     choices = band_names,
                     selected = NULL),
          selectInput(ns("blueBand"), "Blue Band",
                     choices = band_names,
                     selected = NULL)
        )
      )
    })
    
    # Search assets when button is clicked
    observeEvent(input$searchAssets, {
      req(input$collection, input$dateRangeStac, mapBounds())
      log_info("Searching for assets")
      
      tryCatch({
        bbox <- as.numeric(c(
          mapBounds()$west,
          mapBounds()$south,
          mapBounds()$east,
          mapBounds()$north
        ))
        
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
        log_info(sprintf("Found %d assets", length(items$features)))
        
      }, error = function(e) {
        log_error(sprintf("Error searching assets: %s", e$message))
        showNotification(
          paste("Error searching assets:", e$message),
          type = "error"
        )
      })
    })
    
    # Asset selector UI
    output$assetSelector <- renderUI({
      req(rv$availableAssets)
      
      if (length(rv$availableAssets$features) > 0) {
        choices <- sapply(rv$availableAssets$features, function(x) {
          format(as.POSIXct(x$properties$datetime), "%Y-%m-%d %H:%M:%S")
        })
        
        selectInput(ns("selectedAsset"), "Select Asset by Date",
                   choices = choices,
                   selected = NULL)
      } else {
        div(
          class = "alert alert-warning",
          "No assets found for the selected criteria"
        )
      }
    })
    
    # Load asset to map
    observeEvent(input$loadToMap, {
      req(input$selectedAsset, rv$availableAssets)
      if (!input$viewAsRGB) {
        req(input$bandsStac)
      } else {
        req(input$redBand, input$greenBand, input$blueBand)
      }
      log_info("Loading asset to map")
      
      selected_feature <- Filter(
        function(x) format(as.POSIXct(x$properties$datetime), "%Y-%m-%d %H:%M:%S") == input$selectedAsset,
        rv$availableAssets$features
      )[[1]]
      
      if (!is.null(selected_feature)) {
        processAndLoadAsset(input, selected_feature, mapBounds(), rv, map)
      }
    })
  })
}

# Helper Functions
processAndLoadAsset <- function(input, selected_feature, mapBounds, rv, map) {
  tryCatch({
    log_info("Processing selected asset")
    
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
    
    # Create extent
    map_extent <- terra::ext(
      mapBounds$west,
      mapBounds$east,
      mapBounds$south,
      mapBounds$north
    )
    map_bbox <- terra::vect(map_extent, crs="EPSG:4326")
    
    asset_date <- format(as.POSIXct(selected_feature$properties$datetime), "%Y-%m-%d")
    
    if (!input$viewAsRGB) {
      # Single band processing
      loadSingleBands(input, asset_urls, asset_date, map_bbox, rv, map)
    } else {
      # RGB composite processing
      loadRGBComposite(input, asset_urls, asset_date, map_bbox, rv, map)
    }
    
    showNotification("Asset loaded successfully", type = "message")
    
  }, error = function(e) {
    log_error(sprintf("Error processing asset: %s", e$message))
    showNotification(
      paste("Error processing asset:", e$message),
      type = "error"
    )
  })
}

loadSingleBands <- function(input, asset_urls, asset_date, map_bbox, rv, map) {
  new_layers <- paste(input$collection, names(asset_urls), asset_date, sep = "_")
  rv$overlayGroups <- unique(c(rv$overlayGroups, new_layers))
  
  map %>%
    clearGroup(new_layers) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      overlayGroups = rv$overlayGroups,
      options = layersControlOptions(collapsed = FALSE),
      position = "topright"
    )
  
  for (band_name in names(asset_urls)) {
    url <- paste0("/vsicurl/", asset_urls[[band_name]])
    rast <- processRaster(url, map_bbox)
    
    layer_name <- paste(input$collection, band_name, asset_date, sep = "_")
    
    map %>%
      addRasterImage(
        rast,
        layerId = layer_name,
        group = layer_name,
        opacity = 1,
        project = TRUE,
        method = "bilinear"
      ) %>%
      showGroup(layer_name)
  }
}

loadRGBComposite <- function(input, asset_urls, asset_date, map_bbox, rv, map) {
  rgb_layer_name <- paste(input$collection, "RGB", asset_date, sep = "_")
  rv$overlayGroups <- unique(c(rv$overlayGroups, rgb_layer_name))
  
  map %>%
    clearGroup(rgb_layer_name) %>%
    addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellite"),
      overlayGroups = rv$overlayGroups,
      options = layersControlOptions(collapsed = FALSE),
      position = "topright"
    )
  
  rgb_rasts <- lapply(asset_urls, function(url) {
    processRaster(paste0("/vsicurl/", url), map_bbox)
  })
  
  rgb_raster <- brick(stack(rgb_rasts))
  
  map %>%
    addRasterRGB(
      rgb_raster,
      r = 1,
      g = 2,
      b = 3,
      layerId = rgb_layer_name,
      group = rgb_layer_name,
      opacity = 0.9
    ) %>%
    showGroup(rgb_layer_name)
}

processRaster <- function(url, map_bbox) {
  rast <- terra::rast(url)
  
  if (!is.na(terra::crs(rast)) && terra::crs(rast) != "EPSG:4326") {
    map_bbox_transformed <- terra::project(map_bbox, terra::crs(rast))
    rast_cropped <- terra::crop(rast, map_bbox_transformed)
    return(terra::project(rast_cropped, "EPSG:4326"))
  } else {
    return(terra::crop(rast, map_bbox))
  }
} 