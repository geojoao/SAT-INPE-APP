# Módulo MapBiomas - Distribuição de área por classes de cobertura
# Usa a geometria desenhada no mapa para obter estatísticas do MapBiomas
# Limite: área da geometria < 1.000.000 ha
# Classes e cores obtidas da API (apenas itens folha para evitar dupla contagem)

source("mapbiomas/mapbiomas_client.R")

# Cliente MapBiomas
mapbiomas_client <- MapBiomasAPIClient$new()

# Converte feature do Leaflet (draw) para geometria MapBiomas
leaflet_feature_to_mapbiomas_geom <- function(feature) {
  if (is.null(feature) || is.null(feature$geometry)) return(NULL)
  feature_type <- feature$properties$feature_type
  coords <- feature$geometry$coordinates

  if (feature_type == "polygon") {
    return(list(type = "Polygon", coordinates = coords))
  }
  if (feature_type == "marker") {
    center <- unlist(coords)
    point <- st_point(c(center[1], center[2]))
    sf_point <- st_sfc(point, crs = 4326)
    buffered <- st_buffer(sf_point, dist = 0.01)
    m <- st_coordinates(buffered[[1]])
    ring <- lapply(seq_len(nrow(m)), function(i) as.numeric(m[i, 1:2]))
    return(list(type = "Polygon", coordinates = list(ring)))
  }
  if (feature_type == "circle") {
    center <- unlist(coords)
    radius <- feature$properties$radius %||% 5000
    point <- st_point(c(center[1], center[2]))
    sf_point <- st_sfc(point, crs = 4326)
    buffered <- st_buffer(sf_point, dist = radius / 111000)
    m <- st_coordinates(buffered[[1]])
    ring <- lapply(seq_len(nrow(m)), function(i) as.numeric(m[i, 1:2]))
    return(list(type = "Polygon", coordinates = list(ring)))
  }
  NULL
}

`%||%` <- function(x, y) if (is.null(x)) y else x

# UI do módulo MapBiomas
mapbiomasUI <- function(id) {
  ns <- NS(id)
  div(
    hr(),
    uiOutput(ns("file_ui")),
    sliderInput(ns("yearRange"), "Período (anos)",
                min = 1985, max = 2024, value = c(2015, 2024),
                step = 1, sep = ""),
    actionButton(ns("getAreaDistribution"), "Obter Distribuição de Área",
                 class = "btn-primary btn-block",
                 style = "margin-top: 20px;")
  )
}

# Server do módulo MapBiomas
mapbiomasServer <- function(id, leaflet_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy

    rv <- reactiveValues(
      userGeometry = NULL,
      areaTimeSeriesData = NULL,
      currentPlot = NULL,
      legendLeafItems = NULL
    )

    output$file_ui <- renderUI({
      fileInput(ns("kmlUpload"), "Upload KML ou desenhe no mapa", accept = ".kml")
    })

    observeEvent(mainInput$map_draw_new_feature, {
      rv$userGeometry <- mainInput$map_draw_new_feature
    })

    observeEvent(mainInput$map_draw_deleted_features, {
      rv$userGeometry <- NULL
      map %>% clearShapes()
      output$file_ui <- renderUI({
        fileInput(ns("kmlUpload"), "Upload KML ou desenhe no mapa", accept = ".kml")
      })
    })

    observeEvent(input$kmlUpload, {
      req(input$kmlUpload$datapath)
      kml_path <- input$kmlUpload$datapath
      geom_sf <- tryCatch({
        st_read(kml_path) %>% st_zm() %>% st_cast("MULTIPOLYGON")
      }, error = function(e) {
        showNotification("Erro ao ler arquivo KML.", type = "error")
        return(NULL)
      })
      if (is.null(geom_sf)) return
      if (nrow(geom_sf) > 1) {
        geom_sf <- st_sf(geometry = st_union(geom_sf$geometry))
      }
      geom_sf <- st_transform(geom_sf, 4326)

      map %>%
        clearShapes() %>%
        addPolygons(
          data = geom_sf,
          group = "Features",
          fillColor = "green",
          fillOpacity = 0.2,
          color = "green",
          weight = 2
        ) %>%
        flyToBounds(
          lng1 = st_bbox(geom_sf)$xmin[[1]],
          lat1 = st_bbox(geom_sf)$ymin[[1]],
          lng2 = st_bbox(geom_sf)$xmax[[1]],
          lat2 = st_bbox(geom_sf)$ymax[[1]]
        ) %>%
        showGroup("Features")

      rv$userGeometry <- list(source = "sf", geometry = geom_sf)
      showNotification("KML carregado. Clique em 'Obter Distribuição de Área'.", type = "message")
    })

    observeEvent(input$getAreaDistribution, {
      req(rv$userGeometry)
      geom <- if (!is.null(rv$userGeometry$source) && rv$userGeometry$source == "sf") {
        rv$userGeometry$geometry
      } else {
        leaflet_feature_to_mapbiomas_geom(rv$userGeometry)
      }
      if (is.null(geom)) {
        showNotification("Geometria inválida. Desenhe um polígono no mapa.", type = "error")
        return()
      }

      if (is.null(rv$legendLeafItems)) {
        tryCatch({
          rv$legendLeafItems <- mapbiomas_client$get_legend_leaf_items("brazil", "default")
        }, error = function(e) {
          showNotification(paste("Erro ao obter legenda MapBiomas:", e$message), type = "error")
          return()
        })
      }
      legend_df <- rv$legendLeafItems
      if (nrow(legend_df) == 0) {
        showNotification("Legenda MapBiomas vazia.", type = "error")
        return()
      }

      years <- seq(input$yearRange[1], input$yearRange[2])
      pixel_values <- legend_df$pixelValue

      showModal(modalDialog(
        title = "Carregando dados MapBiomas...",
        "Consultando API para cada ano. Aguarde...",
        footer = NULL,
        size = "s"
      ))

      tryCatch({
        all_data <- list()
        n_years <- length(years)
        withProgress(message = "Consultando MapBiomas...", value = 0, {
          for (i in seq_along(years)) {
            setProgress(i / n_years, detail = sprintf("Ano %d/%d", years[i], years[n_years]))
            result <- mapbiomas_client$get_area_statistics(
            region = "brazil",
            subtheme_key = "coverage_lclu",
            legend_key = "default",
            pixel_value = pixel_values,
            year = years[i],
            geometry = geom
          )
          df_year <- parse_area_statistics_to_df(result)
          if (nrow(df_year) > 0) {
            all_data[[i]] <- df_year
          }
        }
        })

        if (length(all_data) == 0) {
          removeModal()
          showNotification("Nenhum dado retornado para o período.", type = "warning")
          return()
        }

        df_all <- do.call(rbind, all_data)
        df_all <- merge(df_all, legend_df[, c("pixelValue", "name", "color")], by = "pixelValue", all.x = TRUE)
        df_all$classe <- df_all$name
        df_all$classe[is.na(df_all$classe)] <- paste0("Classe ", df_all$pixelValue[is.na(df_all$classe)])

        classes_present <- unique(df_all$classe)
        colors_vec <- sapply(classes_present, function(cl) {
          pv <- df_all$pixelValue[df_all$classe == cl][1]
          idx <- which(legend_df$pixelValue == pv)[1]
          if (length(idx) > 0) legend_df$color[idx] else "#999999"
        })
        names(colors_vec) <- classes_present

        p <- plot_ly(df_all, x = ~year, y = ~area_ha, color = ~classe, colors = colors_vec,
                     type = "bar", marker = list(line = list(color = "white", width = 0.5))) %>%
          layout(
            barmode = "stack",
            title = "Distribuição de Área por Classe de Cobertura (MapBiomas)",
            xaxis = list(title = "Ano"),
            yaxis = list(title = "Área (hectares)"),
            legend = list(
              orientation = "v",
              x = 1.02,
              y = 1,
              xanchor = "left",
              yanchor = "top"
            ),
            margin = list(b = 80, t = 60, r = 220)
          )

        rv$currentPlot <- p
        removeModal()
        showModal(modalDialog(
          title = "Série Temporal - Distribuição de Área por Classe",
          plotlyOutput(ns("areaPlot"), height = "500px"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Fechar")
        ))

      }, error = function(e) {
        removeModal()
        showNotification(
          paste("Erro ao obter dados MapBiomas:", e$message),
          type = "error"
        )
      })
    })

    output$areaPlot <- renderPlotly({
      req(rv$currentPlot)
      rv$currentPlot
    })
  })
}
