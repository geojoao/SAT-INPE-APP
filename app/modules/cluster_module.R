library(shiny)
library(plotly)
library(leaflet)
library(sf)
library(terra)
library(rstac)
library(logger)

# STAC client and collections (reuse same endpoint as image viewer)
stac_obj_cluster <- stac("https://data.inpe.br/bdc/stac/v1/")
collections_cluster <- stac_obj_cluster %>%
  collections() %>%
  get_request()

available_collections_cluster <- sapply(collections_cluster$collections, function(x) x$id)

default_collection_cluster <- if ("S2-16D-2" %in% available_collections_cluster) {
  "S2-16D-2"
} else if (length(available_collections_cluster) > 0) {
  available_collections_cluster[1]
} else {
  NULL
}

# Helper: converter geometria compartilhada para sf (para recorte de raster)
shared_geom_to_sf <- function(shared_geom) {
  if (is.null(shared_geom)) return(NULL)
  
  # Caso vindo de KML (app.R marca com source = "sf")
  if (!is.null(shared_geom$source) && shared_geom$source == "sf") {
    geom_sf <- shared_geom$geometry
    # garante crs
    if (is.null(sf::st_crs(geom_sf))) {
      geom_sf <- sf::st_set_crs(geom_sf, 4326)
    } else if (sf::st_crs(geom_sf)$epsg != 4326) {
      geom_sf <- sf::st_transform(geom_sf, 4326)
    }
    return(geom_sf)
  }
  
  # Caso vindo do draw do leaflet (feature GeoJSON-like)
  if (!is.null(shared_geom$geometry) && !is.null(shared_geom$geometry$type)) {
    geom_type <- shared_geom$geometry$type
    coords <- shared_geom$geometry$coordinates
    
    if (identical(geom_type, "Polygon")) {
      ring <- coords[[1]]
      # garante que todos os pontos sejam numericos (lon, lat)
      ring_num <- lapply(ring, function(pt) as.numeric(pt[1:2]))
      mat <- do.call(rbind, ring_num)
      poly <- sf::st_polygon(list(mat))
      geom_sf <- sf::st_sfc(poly, crs = 4326)
      return(sf::st_sf(geometry = geom_sf))
    }
    
    if (identical(geom_type, "MultiPolygon")) {
      polys <- lapply(coords, function(ring_list) {
        ring <- ring_list[[1]]
        ring_num <- lapply(ring, function(pt) as.numeric(pt[1:2]))
        mat <- do.call(rbind, ring_num)
        sf::st_polygon(list(mat))
      })
      geom_sf <- sf::st_sfc(sf::st_multipolygon(polys), crs = 4326)
      return(sf::st_sf(geometry = geom_sf))
    }
  }
  
  NULL
}

# UI do módulo de clusterização
clusterUI <- function(id) {
  ns <- NS(id)
  
  div(
    hr(),
    selectInput(
      ns("collection"),
      "Select Collection (STAC)",
      choices = available_collections_cluster,
      selected = default_collection_cluster
    ),
    uiOutput(ns("bandSelectorCluster")),
    dateRangeInput(
      ns("dateRangeCluster"),
      "Select Date Range",
      start = Sys.Date() - 365,
      end = Sys.Date(),
      format = "yyyy-mm-dd"
    ),
    actionButton(
      ns("searchAssetsCluster"),
      "Search Assets",
      class = "btn-primary btn-block",
      style = "margin-top: 15px;"
    ),
    hr(),
    uiOutput(ns("assetSelectorCluster")),
    numericInput(
      ns("numClusters"),
      "Number of Clusters (k)",
      value = 4,
      min = 2,
      max = 20,
      step = 1
    ),
    actionButton(
      ns("runClustering"),
      "Run Clusterization",
      class = "btn-success btn-block",
      style = "margin-top: 10px;"
    )
  )
}

# Server do módulo de clusterização
clusterServer <- function(id, leaflet_map, shared_geometry = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy
    
    rv <- reactiveValues(
      availableAssets = NULL,
      clusterRaster = NULL,
      clusterPalette = NULL,
      clusterTsData = NULL
    )
    
    # Geometria compartilhada (KML ou desenho)
    userGeometrySf <- reactive({
      if (is.null(shared_geometry) || is.null(shared_geometry$userGeometry)) return(NULL)
      shared_geom_to_sf(shared_geometry$userGeometry)
    })
    
    # Seletor dinâmico de bandas
    output$bandSelectorCluster <- renderUI({
      req(input$collection)
      log_info(sprintf("Cluster module - user selected collection: %s", input$collection))
      
      collection <- tryCatch({
        stac_obj_cluster %>%
          collections() %>%
          get_request() %>%
          purrr::pluck("collections") %>%
          purrr::keep(~.$id == input$collection) %>%
          purrr::pluck(1)
      }, error = function(e) {
        log_error(sprintf("Error retrieving collection metadata for cluster module: %s", e$message))
        NULL
      })
      
      band_names <- character(0)
      if (!is.null(collection) && !is.null(collection$summaries$`eo:bands`)) {
        band_names <- sapply(collection$summaries$`eo:bands`, function(x) x$name)
      } else {
        # Fallback: item de exemplo
        sample_item <- tryCatch({
          stac_obj_cluster %>%
            stac_search(collections = input$collection, limit = 1) %>%
            get_request()
        }, error = function(e) {
          log_error(sprintf("Error retrieving sample item for cluster module: %s", e$message))
          NULL
        })
        
        if (!is.null(sample_item) && length(sample_item$features) > 0) {
          band_names <- names(sample_item$features[[1]]$assets)
        }
      }
      
      if (length(band_names) == 0) {
        return(div(
          class = "alert alert-warning",
          "No bands found for this collection."
        ))
      }
      
      default_band <- if ("EVI" %in% band_names) "EVI" else band_names[1]
      
      selectInput(
        ns("bandCluster"),
        "Select Band for Clustering",
        choices = band_names,
        selected = default_band,
        multiple = FALSE
      )
    })
    
    # Busca de assets (usa bounding box da geometria, não apenas o viewport)
    observeEvent(input$searchAssetsCluster, {
      geom_sf <- userGeometrySf()
      req(input$collection, input$dateRangeCluster, geom_sf)
      
      log_info("Cluster module - searching assets")
      
      bbox <- sf::st_bbox(geom_sf)
      bbox_vec <- as.numeric(c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"]))
      
      withProgress(message = "Searching assets for clustering...", value = 0, {
        tryCatch({
          setProgress(0.3, detail = "Querying STAC API...")
          
          datetime <- paste0(
            format(input$dateRangeCluster[1], "%Y-%m-%dT00:00:00Z"),
            "/",
            format(input$dateRangeCluster[2], "%Y-%m-%dT23:59:59Z")
          )
          
          items <- stac_obj_cluster %>%
            stac_search(
              collections = input$collection,
              bbox = bbox_vec,
              datetime = datetime,
              limit = 100
            ) %>%
            get_request()
          
          rv$availableAssets <- items
          n_feats <- if (!is.null(items$features)) length(items$features) else 0
          log_info(sprintf("Cluster module - found %d assets", n_feats))
          
          setProgress(1, detail = "Done!")
        }, error = function(e) {
          log_error(sprintf("Error searching assets for clustering: %s", e$message))
          showNotification(
            paste("Error searching assets for clustering:", e$message),
            type = "error"
          )
        })
      })
    })
    
    # UI para seleção de múltiplos assets (data + nuvens)
    output$assetSelectorCluster <- renderUI({
      req(rv$availableAssets)
      
      feats <- rv$availableAssets$features
      if (is.null(feats) || length(feats) == 0) {
        return(div(
          class = "alert alert-warning",
          "No assets found for the selected criteria and geometry."
        ))
      }
      
      # Ordemos por data
      dates <- sapply(feats, function(x) as.POSIXct(x$properties$datetime))
      ord <- order(dates)
      feats <- feats[ord]
      
      labels <- sapply(feats, function(x) {
        dt <- format(as.POSIXct(x$properties$datetime), "%Y-%m-%d")
        cc <- x$properties$`eo:cloud_cover`
        if (!is.null(cc) && !is.na(cc)) {
          paste0(dt, " (", round(cc, 1), "% clouds)")
        } else {
          paste0(dt, " (-)")
        }
      })
      
      ids <- sapply(feats, function(x) x$id)
      choices <- stats::setNames(ids, labels)
      
      tagList(
        # Mantém a altura do widget fixa e adiciona scroll interno
        tags$style(sprintf(
          "#%s + .selectize-control .selectize-input { max-height: 60px; overflow-y: auto; overflow-x: hidden; }",
          ns("selectedAssetsCluster")
        )),
        selectInput(
          ns("selectedAssetsCluster"),
          "Select Assets (dates) for clustering",
          choices = choices,
          multiple = TRUE,
          selected = NULL
        )
      )
    })
    
    # Execução do k-means e geração de resultados
    observeEvent(input$runClustering, {
      geom_sf <- userGeometrySf()
      req(geom_sf, input$bandCluster, input$numClusters)
      req(rv$availableAssets, input$selectedAssetsCluster)
      
      feats <- rv$availableAssets$features
      feat_ids <- sapply(feats, function(x) x$id)
      
      # Filtra apenas os assets selecionados
      sel_idx <- which(feat_ids %in% input$selectedAssetsCluster)
      if (length(sel_idx) < 2) {
        showNotification("Please select at least two assets (dates) for clustering.", type = "error")
        return()
      }
      
      feats_sel <- feats[sel_idx]
      # Ordena por data para garantir série temporal correta
      dates_sel <- sapply(feats_sel, function(x) as.POSIXct(x$properties$datetime))
      ord <- order(dates_sel)
      feats_sel <- feats_sel[ord]
      dates_sel <- dates_sel[ord]
      
      k <- as.integer(input$numClusters)
      if (is.na(k) || k < 2) {
        showNotification("Number of clusters (k) must be at least 2.", type = "error")
        return()
      }
      
      # Verifica área aproximada da geometria para evitar processamento exagerado
      area_m2 <- tryCatch({
        geom_eq <- sf::st_transform(geom_sf, 3857)
        as.numeric(sf::st_area(geom_eq))
      }, error = function(e) NA_real_)
      
      if (!is.na(area_m2) && area_m2 > 5e9) { # ~5.000 km2
        showNotification("Selected geometry area is too large for clustering. Please use a smaller area.", type = "error")
        return()
      }
      
      showModal(modalDialog(
        title = "Running clusterization",
        "Please wait while assets are downloaded, cropped and clustered...",
        footer = NULL,
        size = "s",
        easyClose = FALSE
      ))
      
      # Conversão para terra::vect
      geom_vect <- terra::vect(geom_sf)
      
      # Carrega e recorta os rasters
      rasters_list <- list()
      
      tryCatch({
        for (i in seq_along(feats_sel)) {
          feat <- feats_sel[[i]]
          band_name <- input$bandCluster
          if (is.null(feat$assets[[band_name]])) {
            stop(sprintf("Band %s not found in selected asset.", band_name))
          }
          href <- feat$assets[[band_name]]$href
          url <- if (grepl("^/vsi", href)) href else paste0("/vsicurl/", href)
          
          log_info(sprintf("Cluster module - loading raster %s (%s)", url, band_name))
          
          r <- terra::rast(url)
          
          # Garante projeção da geometria compatível
          if (!is.na(terra::crs(r))) {
            if (terra::crs(geom_vect) != terra::crs(r)) {
              geom_v_proj <- terra::project(geom_vect, terra::crs(r))
            } else {
              geom_v_proj <- geom_vect
            }
          } else {
            geom_v_proj <- geom_vect
          }
          
          r_crop <- terra::crop(r, geom_v_proj)
          r_mask <- terra::mask(r_crop, geom_v_proj)
          
          # Tratamento especial para S2-16D-2 se funções do Image Viewer estiverem disponíveis
          if (exists("apply_bdc_s2_reflectance", mode = "function") &&
              identical(input$collection, "S2-16D-2")) {
            r_mask <- apply_bdc_s2_reflectance(r_mask)
          }
          
          rasters_list[[length(rasters_list) + 1]] <- r_mask
        }
        
        # Empilha em um único objeto terra (cada layer = um tempo)
        raster_stack <- terra::rast(rasters_list)
        
        # Extrai valores por pixel (linha = pixel, coluna = tempo)
        vals <- terra::values(raster_stack, na.rm = FALSE)
        if (is.null(vals) || nrow(vals) == 0) {
          stop("No valid pixels found inside the selected geometry.")
        }
        
        valid_idx <- which(stats::complete.cases(vals))
        if (length(valid_idx) < k) {
          stop("Not enough valid pixels inside geometry for the requested number of clusters.")
        }
        
        x <- vals[valid_idx, , drop = FALSE]
        
        set.seed(123)
        km <- stats::kmeans(x, centers = k)
        
        clusters_all <- rep(NA_integer_, nrow(vals))
        clusters_all[valid_idx] <- km$cluster
        
        # Raster de clusters
        cluster_raster <- raster_stack[[1]]
        terra::values(cluster_raster) <- clusters_all
        
        # Área por cluster (ha), usando o tamanho real do pixel
        cluster_areas_ha <- tryCatch({
          cs <- terra::cellSize(cluster_raster, unit = "m")
          z <- terra::zonal(cs, cluster_raster, fun = "sum", na.rm = TRUE)
          areas <- rep(NA_real_, k)
          if (!is.null(z) && nrow(z) > 0) {
            match_idx <- match(seq_len(k), z[, 1])
            valid <- !is.na(match_idx)
            areas[valid] <- z[match_idx[valid], 2] / 10000  # m² -> ha
          }
          areas
        }, error = function(e) {
          log_warn(sprintf("Cluster module - error computing cluster areas: %s", e$message))
          rep(NA_real_, k)
        })
        
        # Série temporal média por cluster
        n_time <- ncol(x)
        means_mat <- matrix(NA_real_, nrow = k, ncol = n_time)
        for (ci in seq_len(k)) {
          idx_ci <- which(km$cluster == ci)
          if (length(idx_ci) > 0) {
            means_mat[ci, ] <- colMeans(x[idx_ci, , drop = FALSE], na.rm = TRUE)
          }
        }
        
        dates_vec <- as.Date(dates_sel)
        cluster_labels <- if (all(is.na(cluster_areas_ha))) {
          paste0("Cluster ", seq_len(k))
        } else {
          paste0(
            "Cluster ", seq_len(k),
            " (", round(cluster_areas_ha, 1), " ha)"
          )
        }
        
        df_ts <- data.frame(
          date = rep(dates_vec, times = k),
          cluster = factor(
            rep(seq_len(k), each = length(dates_vec)),
            levels = seq_len(k),
            labels = cluster_labels
          ),
          mean_value = as.vector(t(means_mat))
        )
        
        # Paleta de cores compartilhada entre mapa e série temporal
        palette_colors <- grDevices::rainbow(k)
        pal_leaflet <- leaflet::colorFactor(palette_colors, domain = seq_len(k), na.color = "transparent")
        
        rv$clusterRaster <- cluster_raster
        rv$clusterPalette <- list(colors = palette_colors, pal = pal_leaflet)
        rv$clusterTsData <- df_ts
        
        # Adiciona a camada de clusters ao mapa principal
        layer_name <- paste("Clusters", input$collection, input$bandCluster, sep = "_")
        
        map %>%
          leaflet::clearGroup(layer_name) %>%
          leaflet::addRasterImage(
            cluster_raster,
            colors = pal_leaflet,
            group = layer_name,
            layerId = layer_name,
            opacity = 0.7,
            project = TRUE,
            method = "ngb"
          ) %>%
          leaflet::showGroup(layer_name)
        
        removeModal()
        
        # Abre modal de resultados
        showModal(modalDialog(
          title = "Clusterization Results",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          fluidRow(
            column(
              width = 6,
              plotlyOutput(ns("clusterTsPlot"), height = "400px")
            ),
            column(
              width = 6,
              leafletOutput(ns("clusterMapPlot"), height = "400px")
            )
          )
        ))
        
      }, error = function(e) {
        log_error(sprintf("Error running clustering: %s", e$message))
        removeModal()
        showNotification(
          paste("Error running clustering:", e$message),
          type = "error"
        )
      })
    })
    
    # Plot série temporal média por cluster
    output$clusterTsPlot <- renderPlotly({
      req(rv$clusterTsData, rv$clusterPalette)
      df <- rv$clusterTsData
      cols <- rv$clusterPalette$colors
      
      p <- plotly::plot_ly(df, x = ~date, y = ~mean_value,
                           color = ~cluster,
                           colors = cols,
                           type = "scatter",
                           mode = "lines") %>%
        layout(
          title = "Mean time series per cluster",
          xaxis = list(title = "Date"),
          yaxis = list(title = "Value"),
          legend = list(
            orientation = "v",
            x = 1.05,
            y = 0.5,
            xanchor = "left",
            yanchor = "middle"
          )
        )
      p
    })
    
    # Mapa de clusters (renderizado em modal)
    output$clusterMapPlot <- renderLeaflet({
      req(rv$clusterRaster, rv$clusterPalette)
      r <- rv$clusterRaster
      pal_leaflet <- rv$clusterPalette$pal
      
      leaflet() %>%
        addTiles() %>%
        addRasterImage(
          r,
          colors = pal_leaflet,
          opacity = 0.7,
          project = TRUE,
          method = "ngb"
        )
    })
  })
}

