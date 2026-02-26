# Modulo MapBiomas - Distribuicao de Area por classes de cobertura
# Usa a geometria desenhada no mapa para obter estatisticas do MapBiomas
# Limite: Area da geometria < 1.000.000 ha
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

# UI do submodulo MapBiomas - Agricultura
mapbiomasAgricultureUI <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("file_ui")),
    sliderInput(ns("yearRange"), "Periodo (anos)",
                min = 1985, max = 2024, value = c(2015, 2024),
                step = 1, sep = ""),
    actionButton(ns("getAreaDistribution"), "Obter Distribuicao de Area",
                 class = "btn-primary btn-block",
                 style = "margin-top: 20px;")
  )
}

# Constantes para submódulo Solos (MapBiomas API)
SOIL_FRACTION_OPTS <- list(
  Sand = list(subtheme = "soil_sand_fraction", legend = "soil_sand_fraction_mapbiomas_sand"),
  Silt = list(subtheme = "soil_silt_fraction", legend = "soil_silt_fraction_mapbiomas_silt"),
  Clay = list(subtheme = "soil_clay_fraction", legend = "soil_clay_fraction_mapbiomas_clay")
)
SOIL_TEXTURE_OPTS <- list(
  "Textural group" = list(subtheme = "soil_textural_group", legend = "soil_textural_group_mapbiomas_textural_group"),
  "Textural subgroup" = list(subtheme = "soil_textural_subgroup", legend = "soil_textural_subgroup_mapbiomas_textural_subgroup"),
  "Textural class" = list(subtheme = "soil_textural_class", legend = "soil_textural_class_mapbiomas_textural_class")
)

# UI do submodulo MapBiomas - Solos
mapbiomasSoilUI <- function(id) {
  ns <- NS(id)
  div(
    uiOutput(ns("soil_file_ui")),
    tags$h5("Métricas de perfis de solo (frações)", style = "margin-top: 12px;"),
    selectInput(ns("soilFraction"), "Componente",
                choices = c("Sand", "Silt", "Clay"),
                selected = "Clay"),
    selectInput(ns("soilProfileDepth"), "Profundidade (cm)",
                choices = c("0-10" = "000_010", "10-20" = "010_020", "20-30" = "020_030",
                            "30-40" = "030_040", "40-50" = "040_050", "50-60" = "050_060",
                            "60-70" = "060_070", "70-80" = "070_080", "80-90" = "080_090"),
                selected = "000_010"),
    tags$hr(),
    tags$h5("Textura"),
    selectInput(ns("soilTextureType"), "Tipo de textura",
                choices = c("Textural group", "Textural subgroup", "Textural class"),
                selected = "Textural group"),
    selectInput(ns("soilTextureDepth"), "Profundidade (cm)",
                choices = c("0-10" = "000_010", "0-20" = "000_020", "0-30" = "000_030"),
                selected = "000_010"),
    actionButton(ns("getSoilMetrics"), "Obter métricas de solo",
                 class = "btn-primary btn-block", style = "margin-top: 16px;")
  )
}

# UI do modulo MapBiomas (container de submodulos)
mapbiomasUI <- function(id) {
  ns <- NS(id)
  div(
    hr(),
    tabsetPanel(
      id = ns("mapbiomasTabs"),
      tabPanel("Agricultura", mapbiomasAgricultureUI(ns("agriculture"))),
      tabPanel("Solos", mapbiomasSoilUI(ns("soil")))
    )
  )
}

# Server do submodulo MapBiomas - Agricultura
mapbiomasAgricultureServer <- function(id, leaflet_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy

    rv <- reactiveValues(
      userGeometry = NULL,
      areaTimeSeriesData = NULL,
      coveragePlot = NULL,
      secondCropPlot = NULL,
      irrigationPlot = NULL,
      legendLeafItems = NULL,
      secondCropLegendItems = NULL,
      irrigationLegendItems = NULL
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
      showNotification("KML carregado. Clique em 'Obter Distribuicao de Area'.", type = "message")
    })

    observeEvent(input$getAreaDistribution, {
      req(rv$userGeometry)
      geom <- if (!is.null(rv$userGeometry$source) && rv$userGeometry$source == "sf") {
        rv$userGeometry$geometry
      } else {
        leaflet_feature_to_mapbiomas_geom(rv$userGeometry)
      }
      if (is.null(geom)) {
        showNotification("Geometria invAlida. Desenhe um poligono no mapa.", type = "error")
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

      # Legenda para subtema de second crop (safrinha)
      if (is.null(rv$secondCropLegendItems)) {
        tryCatch({
          rv$secondCropLegendItems <- mapbiomas_client$get_legend_leaf_items(
            "brazil",
            "agriculture_agricultural_use_second_crop_mapbiomas_agricultural_use_second_crop"
          )
        }, error = function(e) {
          showNotification(paste("Erro ao obter legenda de second crop (MapBiomas):", e$message), type = "error")
          return()
        })
      }
      second_legend_df <- rv$secondCropLegendItems

      # Legenda para sistemas de irrigacao
      if (is.null(rv$irrigationLegendItems)) {
        tryCatch({
          rv$irrigationLegendItems <- mapbiomas_client$get_legend_leaf_items(
            "brazil",
            "agriculture_irrigation_systems_mapbiomas_irrigation_systems"
          )
        }, error = function(e) {
          showNotification(paste("Erro ao obter legenda de irrigacao (MapBiomas):", e$message), type = "error")
          return()
        })
      }
      irrigation_legend_df <- rv$irrigationLegendItems

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
        second_all_data <- list()
        irrigation_all_data <- list()
        n_years <- length(years)
        withProgress(message = "Consultando MapBiomas...", value = 0, {
          for (i in seq_along(years)) {
            setProgress(i / n_years, detail = sprintf("Ano %d/%d", years[i], years[n_years]))

            # Cobertura / uso (coverage_lclu)
            result_cov <- mapbiomas_client$get_area_statistics(
              region = "brazil",
              subtheme_key = "coverage_lclu",
              legend_key = "default",
              pixel_value = pixel_values,
              year = years[i],
              geometry = geom
            )
            df_year_cov <- parse_area_statistics_to_df(result_cov)
            if (nrow(df_year_cov) > 0) {
              all_data[[i]] <- df_year_cov
            }

            # Second crop (safrinha)
            result_second <- mapbiomas_client$get_area_statistics(
              region = "brazil",
              subtheme_key = "agriculture_agricultural_use_second_crop",
              legend_key = "agriculture_agricultural_use_second_crop_mapbiomas_agricultural_use_second_crop",
              pixel_value = c(1, 62, 41),
              year = years[i],
              geometry = geom
            )
            df_year_second <- parse_area_statistics_to_df(result_second)
            if (nrow(df_year_second) > 0) {
              second_all_data[[i]] <- df_year_second
            }

            # Sistemas de irrigacao
            if (nrow(irrigation_legend_df) > 0) {
              irrigation_pixels <- irrigation_legend_df$pixelValue
              result_irrig <- mapbiomas_client$get_area_statistics(
                region = "brazil",
                subtheme_key = "agriculture_irrigation_systems",
                legend_key = "agriculture_irrigation_systems_mapbiomas_irrigation_systems",
                pixel_value = irrigation_pixels,
                year = years[i],
                geometry = geom
              )
              df_year_irrig <- parse_area_statistics_to_df(result_irrig)
              if (nrow(df_year_irrig) > 0) {
                irrigation_all_data[[i]] <- df_year_irrig
              }
            }
          }
        })

        if (length(all_data) == 0) {
          removeModal()
          showNotification("Nenhum dado retornado para o periodo.", type = "warning")
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

        p_cov <- plot_ly(df_all, x = ~year, y = ~area_ha, color = ~classe, colors = colors_vec,
                         type = "bar", marker = list(line = list(color = "white", width = 0.5))) %>%
          layout(
            barmode = "stack",
            title = "Distribuicao de Area por Classe de Cobertura (MapBiomas)",
            xaxis = list(title = "Ano"),
            yaxis = list(title = "Area (hectares)"),
            legend = list(
              orientation = "v",
              x = 1.02,
              y = 1,
              xanchor = "left",
              yanchor = "top"
            ),
            margin = list(b = 80, t = 60, r = 220)
          )

        rv$coveragePlot <- p_cov

        # Preparar grafico de safrinha (second crop), se houver dados
        if (length(second_all_data) > 0 && nrow(second_legend_df) > 0) {
          df_second_all <- do.call(rbind, second_all_data)
          df_second_all <- merge(
            df_second_all,
            second_legend_df[, c("pixelValue", "name", "color")],
            by = "pixelValue",
            all.x = TRUE
          )
          df_second_all$classe <- df_second_all$name
          df_second_all$classe[is.na(df_second_all$classe)] <- paste0("Classe ", df_second_all$pixelValue[is.na(df_second_all$classe)])

          second_classes <- unique(df_second_all$classe)
          second_colors_vec <- sapply(second_classes, function(cl) {
            pv <- df_second_all$pixelValue[df_second_all$classe == cl][1]
            idx <- which(second_legend_df$pixelValue == pv)[1]
            if (length(idx) > 0) second_legend_df$color[idx] else "#999999"
          })
          names(second_colors_vec) <- second_classes

          p_second <- plot_ly(df_second_all, x = ~year, y = ~area_ha, color = ~classe,
                              colors = second_colors_vec,
                              type = "bar",
                              marker = list(line = list(color = "white", width = 0.5))) %>%
            layout(
              barmode = "stack",
              title = "Area de Safrinha (Second Crop) por Categoria",
              xaxis = list(title = "Ano"),
              yaxis = list(title = "Area (hectares)"),
              legend = list(
                orientation = "v",
                x = 1.02,
                y = 1,
                xanchor = "left",
                yanchor = "top"
              ),
              margin = list(b = 80, t = 60, r = 220)
            )

          rv$secondCropPlot <- p_second
        } else {
          rv$secondCropPlot <- NULL
        }

        # Preparar grafico de sistemas de irrigacao, se houver dados
        if (length(irrigation_all_data) > 0 && nrow(irrigation_legend_df) > 0) {
          df_irrig_all <- do.call(rbind, irrigation_all_data)
          df_irrig_all <- merge(
            df_irrig_all,
            irrigation_legend_df[, c("pixelValue", "name", "color")],
            by = "pixelValue",
            all.x = TRUE
          )
          df_irrig_all$classe <- df_irrig_all$name
          df_irrig_all$classe[is.na(df_irrig_all$classe)] <- paste0("Classe ", df_irrig_all$pixelValue[is.na(df_irrig_all$classe)])

          irrig_classes <- unique(df_irrig_all$classe)
          irrig_colors_vec <- sapply(irrig_classes, function(cl) {
            pv <- df_irrig_all$pixelValue[df_irrig_all$classe == cl][1]
            idx <- which(irrigation_legend_df$pixelValue == pv)[1]
            if (length(idx) > 0) irrigation_legend_df$color[idx] else "#999999"
          })
          names(irrig_colors_vec) <- irrig_classes

          p_irrig <- plot_ly(df_irrig_all, x = ~year, y = ~area_ha, color = ~classe,
                             colors = irrig_colors_vec,
                             type = "bar",
                             marker = list(line = list(color = "white", width = 0.5))) %>%
            layout(
              barmode = "stack",
              title = "Area por Tipo de Irrigacao (MapBiomas)",
              xaxis = list(title = "Ano"),
              yaxis = list(title = "Area (hectares)"),
              legend = list(
                orientation = "v",
                x = 1.02,
                y = 1,
                xanchor = "left",
                yanchor = "top"
              ),
              margin = list(b = 80, t = 60, r = 220)
            )

          rv$irrigationPlot <- p_irrig
        } else {
          rv$irrigationPlot <- NULL
        }

        removeModal()
        showModal(modalDialog(
          title = "Series Temporais - MapBiomas",
          plotlyOutput(ns("areaPlot"), height = "400px"),
          tags$hr(),
          plotlyOutput(ns("secondCropPlot"), height = "400px"),
          tags$hr(),
          plotlyOutput(ns("irrigationPlot"), height = "400px"),
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
      req(rv$coveragePlot)
      rv$coveragePlot
    })

    output$secondCropPlot <- renderPlotly({
      req(rv$secondCropPlot)
      rv$secondCropPlot
    })

    output$irrigationPlot <- renderPlotly({
      req(rv$irrigationPlot)
      rv$irrigationPlot
    })
  })
}

# Server do submodulo MapBiomas - Solos
mapbiomasSoilServer <- function(id, leaflet_map) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy

    rv_soil <- reactiveValues(
      userGeometry = NULL,
      profilePlot = NULL,
      texturePlot = NULL
    )

    output$soil_file_ui <- renderUI({
      fileInput(ns("soilKmlUpload"), "Upload KML ou desenhe no mapa", accept = ".kml")
    })

    observeEvent(mainInput$map_draw_new_feature, {
      rv_soil$userGeometry <- mainInput$map_draw_new_feature
    })

    observeEvent(mainInput$map_draw_deleted_features, {
      rv_soil$userGeometry <- NULL
      map %>% clearShapes()
      output$soil_file_ui <- renderUI({
        fileInput(ns("soilKmlUpload"), "Upload KML ou desenhe no mapa", accept = ".kml")
      })
    })

    observeEvent(input$soilKmlUpload, {
      req(input$soilKmlUpload$datapath)
      kml_path <- input$soilKmlUpload$datapath
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
      rv_soil$userGeometry <- list(source = "sf", geometry = geom_sf)
      showNotification("KML carregado. Use os botoes para obter area.", type = "message")
    })

    # Obter ambas as métricas (perfis + textura) e exibir donuts lado a lado
    observeEvent(input$getSoilMetrics, {
      req(rv_soil$userGeometry)
      geom <- if (!is.null(rv_soil$userGeometry$source) && rv_soil$userGeometry$source == "sf") {
        rv_soil$userGeometry$geometry
      } else {
        leaflet_feature_to_mapbiomas_geom(rv_soil$userGeometry)
      }
      if (is.null(geom)) {
        showNotification("Geometria invalida. Desenhe um poligono no mapa.", type = "error")
        return()
      }
      showModal(modalDialog("Carregando métricas de solo...", footer = NULL, size = "s"))
      tryCatch({
        # 1) Perfis (fração)
        frac_opt <- SOIL_FRACTION_OPTS[[input$soilFraction]]
        depth_profile <- input$soilProfileDepth
        legend_frac <- mapbiomas_client$get_legend_leaf_items("brazil", frac_opt$legend)
        if (nrow(legend_frac) == 0) stop("Legenda de perfis de solo nao disponivel.")
        result_frac <- mapbiomas_client$get_area_statistics(
          region = "brazil",
          subtheme_key = frac_opt$subtheme,
          legend_key = frac_opt$legend,
          pixel_value = legend_frac$pixelValue,
          year = NULL,
          geometry = geom,
          filters = list(depth = depth_profile),
          spatial_method = "union"
        )
        df_frac <- parse_area_statistics_to_df(result_frac)
        df_frac <- merge(df_frac, legend_frac[, c("pixelValue", "name", "color")], by = "pixelValue", all.x = TRUE)
        df_frac$classe <- df_frac$name
        df_frac$classe[is.na(df_frac$classe)] <- paste0("Classe ", df_frac$pixelValue[is.na(df_frac$classe)])
        p_frac <- plot_ly(df_frac, labels = ~classe, values = ~area_ha, type = "pie", hole = 0.5,
                          marker = list(colors = df_frac$color),
                          textinfo = "label+percent",
                          textposition = "outside",
                          hoverinfo = "label+value+percent") %>%
          layout(
            title = list(
              text = sprintf("%s - Prof. %s cm", input$soilFraction, gsub("_", "-", depth_profile)),
              font = list(size = 14)
            ),
            showlegend = TRUE,
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
            margin = list(t = 50, b = 80),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          )

        # 2) Textura
        tex_opt <- SOIL_TEXTURE_OPTS[[input$soilTextureType]]
        depth_tex <- input$soilTextureDepth
        legend_tex <- mapbiomas_client$get_legend_leaf_items("brazil", tex_opt$legend)
        if (nrow(legend_tex) == 0) stop("Legenda de textura nao disponivel.")
        result_tex <- mapbiomas_client$get_area_statistics(
          region = "brazil",
          subtheme_key = tex_opt$subtheme,
          legend_key = tex_opt$legend,
          pixel_value = legend_tex$pixelValue,
          year = NULL,
          geometry = geom,
          filters = list(depth = depth_tex),
          spatial_method = "union"
        )
        df_tex <- parse_area_statistics_to_df(result_tex)
        df_tex <- merge(df_tex, legend_tex[, c("pixelValue", "name", "color")], by = "pixelValue", all.x = TRUE)
        df_tex$classe <- df_tex$name
        df_tex$classe[is.na(df_tex$classe)] <- paste0("Classe ", df_tex$pixelValue[is.na(df_tex$classe)])
        p_tex <- plot_ly(df_tex, labels = ~classe, values = ~area_ha, type = "pie", hole = 0.5,
                         marker = list(colors = df_tex$color),
                         textinfo = "label+percent",
                         textposition = "outside",
                         hoverinfo = "label+value+percent") %>%
          layout(
            title = list(
              text = sprintf("%s - Prof. %s cm", input$soilTextureType, gsub("_", "-", depth_tex)),
              font = list(size = 14)
            ),
            showlegend = TRUE,
            legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.15),
            margin = list(t = 50, b = 80),
            paper_bgcolor = "transparent",
            plot_bgcolor = "transparent"
          )

        rv_soil$profilePlot <- p_frac
        rv_soil$texturePlot <- p_tex
        removeModal()
        showModal(modalDialog(
          title = "Métricas de solo - Distribuição de área",
          fluidRow(
            column(6, plotlyOutput(ns("soilProfilePlot"), height = "420px")),
            column(6, plotlyOutput(ns("soilTexturePlot"), height = "420px"))
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Fechar")
        ))
      }, error = function(e) {
        removeModal()
        showNotification(paste("Erro ao obter métricas de solo:", e$message), type = "error")
      })
    })

    output$soilProfilePlot <- renderPlotly({
      req(rv_soil$profilePlot)
      rv_soil$profilePlot
    })

    output$soilTexturePlot <- renderPlotly({
      req(rv_soil$texturePlot)
      rv_soil$texturePlot
    })
  })
}

# Server do modulo MapBiomas (container)
mapbiomasServer <- function(id, leaflet_map) {
  moduleServer(id, function(input, output, session) {
    mapbiomasAgricultureServer("agriculture", leaflet_map)
    mapbiomasSoilServer("soil", leaflet_map)
  })
}