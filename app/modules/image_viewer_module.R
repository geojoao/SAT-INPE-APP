# Initialize STAC client and get collections
stac_obj <- stac("https://data.inpe.br/bdc/stac/v1/")
collections <- stac_obj %>%
  collections() %>%
  get_request()

available_collections <- sapply(collections$collections, function(x) x$id)

# Default collection: Sentinel-2 (S2-16D-2)
default_collection <- if ("S2-16D-2" %in% available_collections) {
  "S2-16D-2"
} else {
  idx <- grep("^S2|sentinel-2", available_collections, ignore.case = TRUE)
  if (length(idx) > 0) available_collections[idx[1]] else available_collections[1]
}

# INPE BDC S2-16D-2: escala 0.0001, NoData -9999, reflectncia 0-10000 -> 0-1
# https://brazil-data-cube.github.io/products/cube-collections/S2-16D-2.html
BDC_S2_SCALE <- 0.0001
BDC_S2_NODATA <- -9999

# Aplica tratamento INPE BDC para bandas de reflectncia (RGB): escala, nodata, clip 0-1, leve clareamento
BDC_S2_BRIGHTEN <- 1.5  # fator para clarear um pouco a imagem (1 = sem alterao)

apply_bdc_s2_reflectance <- function(r) {
  vals <- terra::values(r)
  vals[vals <= BDC_S2_NODATA] <- NA
  vals <- vals * BDC_S2_SCALE
  vals <- vals * BDC_S2_BRIGHTEN
  vals <- pmin(pmax(vals, 0), 1)
  terra::values(r) <- vals
  r
}

# Image Viewer Module UI
imageViewerUI <- function(id) {
  ns <- NS(id)
  
  div(
    hr(),
    selectInput(ns("collection"), "Select Collection",
               choices = available_collections,
               selected = default_collection),
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
imageViewerServer <- function(id, leaflet_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    mainInput <- leaflet_map$mapInput
    overlayGroups <- leaflet_map$overlayGroups
    baseGroups <- leaflet_map$baseGroups
    map <- leaflet_map$proxy
    addImageLayer <- leaflet_map$addImageLayer

    rv <- reactiveValues(
      availableAssets = NULL,
      baseGroups = baseGroups,
      overlayGroups = overlayGroups,
      mapBounds = NULL,
    )
    
    # Store map bounds when they change
    observeEvent(mainInput$map_bounds, {
      rv$mapBounds <- mainInput$map_bounds
    })

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
      
      # Default band: EVI for single band; B04, B03, B02 for true color (Sentinel-2)
      default_single <- if ("EVI" %in% band_names) "EVI" else band_names[1]
      default_red <- if ("B04" %in% band_names) "B04" else band_names[1]
      default_green <- if ("B03" %in% band_names) "B03" else band_names[min(2, length(band_names))]
      default_blue <- if ("B02" %in% band_names) "B02" else band_names[min(3, length(band_names))]
      
      tagList(
        checkboxInput(ns("viewAsRGB"), "View as RGB", FALSE),
        conditionalPanel(
          condition = sprintf("input['%s'] == false", ns("viewAsRGB")),
          selectInput(ns("bandsStac"), "Select Bands",
                     choices = band_names,
                     multiple = TRUE,
                     selected = default_single)
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == true", ns("viewAsRGB")),
          selectInput(ns("redBand"), "Red Band",
                     choices = band_names,
                     selected = default_red),
          selectInput(ns("greenBand"), "Green Band",
                     choices = band_names,
                     selected = default_green),
          selectInput(ns("blueBand"), "Blue Band",
                     choices = band_names,
                     selected = default_blue)
        )
      )
    })
    
    # Search assets when button is clicked
    observeEvent(input$searchAssets, {
      req(input$collection, input$dateRangeStac, rv$mapBounds)
      log_info("Searching for assets")
      
      withProgress(message = "Searching assets...", value = 0, {
        tryCatch({
          setProgress(0.2, detail = "Preparing search parameters...")
          bbox <- as.numeric(c(
            rv$mapBounds$west,
            rv$mapBounds$south,
            rv$mapBounds$east,
            rv$mapBounds$north
          ))
          
          datetime <- paste0(
            format(input$dateRangeStac[1], "%Y-%m-%dT00:00:00Z"),
            "/",
            format(input$dateRangeStac[2], "%Y-%m-%dT23:59:59Z")
          )
          
          setProgress(0.5, detail = "Querying STAC API...")
          items <- stac_obj %>%
            stac_search(
              collections = input$collection,
              bbox = bbox,
              datetime = datetime,
              limit = 100
            ) %>%
            get_request()
          
          setProgress(0.9, detail = "Processing results...")
          rv$availableAssets <- items
          log_info(sprintf("Found %d assets", length(items$features)))
          
          setProgress(1, detail = "Done!")
          
        }, error = function(e) {
          log_error(sprintf("Error searching assets: %s", e$message))
          showNotification(
            paste("Error searching assets:", e$message),
            type = "error"
          )
        })
      })
    })
    
    # Asset selector UI (data + percentual de nuvens)
    output$assetSelector <- renderUI({
      req(rv$availableAssets)
      
      if (length(rv$availableAssets$features) > 0) {
        choices <- sapply(rv$availableAssets$features, function(x) {
          dt <- format(as.POSIXct(x$properties$datetime), "%Y-%m-%d")
          cc <- x$properties$`eo:cloud_cover`
          if (!is.null(cc) && !is.na(cc)) {
            paste0(dt, " (", round(cc, 1), "% nuvens)")
          } else {
            paste0(dt, " (-)")
          }
        })
        names(choices) <- NULL
        
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

      # Validate area of map bounds before processing the asset
      req(rv$mapBounds)
      bounds <- rv$mapBounds
      width <- abs(bounds$east - bounds$west)
      height <- abs(bounds$north - bounds$south)
      area <- width * height
      max_area_threshold <- 1  # Define max allowable area in square degrees
      
      if(area > max_area_threshold) {
        showNotification("Selected area is too large. Please zoom in before loading the asset.", type = "error")
        return()
      }
      
      log_info("Loading asset to map")
      
      selected_feature <- Filter(
        function(x) {
          dt <- format(as.POSIXct(x$properties$datetime), "%Y-%m-%d")
          cc <- x$properties$`eo:cloud_cover`
          label <- if (!is.null(cc) && !is.na(cc)) {
            paste0(dt, " (", round(cc, 1), "% nuvens)")
          } else {
            paste0(dt, " (-)")
          }
          label == input$selectedAsset
        },
        rv$availableAssets$features
      )[[1]]
      
      if (!is.null(selected_feature)) {
        withProgress(message = "Loading asset to map...", value = 0, {
          processAndLoadAsset(input, selected_feature, rv, map, session, addImageLayer)
          setProgress(1, detail = "Done!")
        })
      }
    })
  })
}

# Helper Functions
processAndLoadAsset <- function(input, selected_feature, rv, map, session = NULL, addImageLayer = NULL) {
  tryCatch({
    log_info("Processing selected asset")
    
    if (!is.null(session)) {
      setProgress(0.1, detail = "Preparing asset URLs...", session = session)
    }
    
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
    
    if (!is.null(session)) {
      setProgress(0.3, detail = "Creating extent...", session = session)
    }
    
    # Create extent
    map_extent <- terra::ext(
      rv$mapBounds$west,
      rv$mapBounds$east,
      rv$mapBounds$south,
      rv$mapBounds$north
    )
    map_bbox <- terra::vect(map_extent, crs="EPSG:4326")
    
    asset_date <- format(as.POSIXct(selected_feature$properties$datetime), "%Y-%m-%d")
    
    if (!input$viewAsRGB) {
      # Single band processing
      if (!is.null(session)) {
        setProgress(0.5, detail = "Loading and processing raster...", session = session)
      }
      loadSingleBands(input, asset_urls, asset_date, map_bbox, rv, map, addImageLayer)
    } else {
      # RGB composite processing
      if (!is.null(session)) {
        setProgress(0.5, detail = "Loading RGB composite...", session = session)
      }
      loadRGBComposite(input, asset_urls, asset_date, map_bbox, rv, map, addImageLayer)
    }
    
    if (!is.null(session)) {
      setProgress(0.9, detail = "Rendering on map...", session = session)
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

loadSingleBands <- function(input, asset_urls, asset_date, map_bbox, rv, map, addImageLayer = NULL) {
  for (band_name in names(asset_urls)) {
    url <- paste0("/vsicurl/", asset_urls[[band_name]])
    rast <- processRaster(url, map_bbox)

    layer_name <- paste(input$collection, band_name, asset_date, sep = "_")
    rv$baseGroups <- unique(c(rv$baseGroups, layer_name))

    map %>%
      clearGroup(layer_name) %>%
      addRasterImage(
        rast,
        layerId = layer_name,
        group = layer_name,
        opacity = 1,
        project = TRUE,
        method = "bilinear"
      ) %>%
      showGroup(layer_name)

    if (is.function(addImageLayer)) {
      addImageLayer(layer_name, layer_name)
    }
  }
}

loadRGBComposite <- function(input, asset_urls, asset_date, map_bbox, rv, map, addImageLayer = NULL) {
  rgb_layer_name <- paste(input$collection, "RGB", asset_date, sep = "_")
  rv$baseGroups <- unique(c(rv$baseGroups, rgb_layer_name))

  map %>% clearGroup(rgb_layer_name)
  
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
      rast <- terra::project(rast_cropped, "EPSG:4326")
    } else {
      rast <- terra::crop(rast, map_bbox)
    }
    
    # Tratamento INPE BDC S2-16D-2: escala 0.0001, nodata -9999, clip 0-1
    if (identical(input$collection, "S2-16D-2")) {
      rast <- apply_bdc_s2_reflectance(rast)
    }
    rgb_rasts[[i]] <- rast
  }
  
  r <- rgb_rasts[[1]]
  g <- rgb_rasts[[2]]
  b <- rgb_rasts[[3]]

  rgb_raster <- c(r, g, b)
  rgb_raster <- brick(rgb_raster)
  
    # Domnio 0-1 para reflectncia j escalada (INPE BDC)
    map %>%
      addRasterRGB(
        rgb_raster,
        r = 1,
        g = 2,
        b = 3,
        layerId = rgb_layer_name,
        group = rgb_layer_name,
        opacity = 0.9,
        quantiles = NULL,
        domain = c(0, 1)
      ) %>%
      showGroup(rgb_layer_name)

  if (is.function(addImageLayer)) {
    addImageLayer(rgb_layer_name, rgb_layer_name)
  }
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