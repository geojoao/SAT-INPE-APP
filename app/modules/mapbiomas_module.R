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
    sliderInput(ns("yearRange"), "Period (years)",
                min = 1985, max = 2024, value = c(2015, 2024),
                step = 1, sep = ""),
    actionButton(ns("getAreaDistribution"), "Get Area Distribution",
                 class = "btn-primary btn-block",
                 style = "margin-top: 20px;")
  )
}

# Constantes para submdulo Solos (MapBiomas API)
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

# Slope categories: MapBiomas uses EMBRAPA classification (slope %)
SLOPE_PERCENT_RANGES <- c(
  "1" = "0-3%",
  "2" = "3-8%",
  "3" = "8-20%",
  "4" = "20-45%",
  "5" = "45%+"
)

# Environmental Analysis config (Hipsometry, Slope, Aspect)
ENV_ANALYSIS_OPTS <- list(
  Hipsometry = list(
    subtheme = "environmental_analisys_hipsometry",
    legend = "environmental_analisys_hipsometry_mapbiomas_hipsometry",
    pixel_values = 1:8
  ),
  Slope = list(
    subtheme = "environmental_analisys_slope",
    legend = "environmental_analisys_slope_mapbiomas_slope",
    pixel_values = 1:5
  ),
  Aspect = list(
    subtheme = "environmental_analisys_aspect",
    legend = "environmental_analisys_aspect_mapbiomas_aspect",
    pixel_values = 1:10
  )
)

# UI do submodulo MapBiomas - Environmental Analysis
mapbiomasEnvAnalysisUI <- function(id) {
  ns <- NS(id)
  div(
    actionButton(ns("getEnvMetrics"), "Get Hipsometry, Slope and Aspect metrics",
                 class = "btn-primary btn-block",
                 style = "margin-top: 20px;")
  )
}

# UI do submodulo MapBiomas - Atmosphere (precipitacao)
mapbiomasAtmosphereUI <- function(id) {
  ns <- NS(id)
  div(
    actionButton(ns("getPrecipitationPlots"), "Get Precipitation and Temperature Plots",
                 class = "btn-primary btn-block",
                 style = "margin-top: 20px;")
  )
}

# UI do submodulo MapBiomas - Solos
mapbiomasSoilUI <- function(id) {
  ns <- NS(id)
  div(
    tags$h5("Soil profile metrics (fractions)", style = "margin-top: 12px;"),
    selectInput(ns("soilFraction"), "Component",
                choices = c("Sand", "Silt", "Clay"),
                selected = "Clay"),
    selectInput(ns("soilProfileDepth"), "Depth (cm)",
                choices = c("0-10" = "000_010", "10-20" = "010_020", "20-30" = "020_030",
                            "30-40" = "030_040", "40-50" = "040_050", "50-60" = "050_060",
                            "60-70" = "060_070", "70-80" = "070_080", "80-90" = "080_090"),
                selected = "000_010"),
    tags$hr(),
    tags$h5("Texture"),
    selectInput(ns("soilTextureType"), "Texture type",
                choices = c("Textural group", "Textural subgroup", "Textural class"),
                selected = "Textural group"),
    selectInput(ns("soilTextureDepth"), "Depth (cm)",
                choices = c("0-10" = "000_010", "0-20" = "000_020", "0-30" = "000_030"),
                selected = "000_010"),
    actionButton(ns("getSoilMetrics"), "Get soil metrics",
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
      tabPanel("Agriculture", mapbiomasAgricultureUI(ns("agriculture"))),
      tabPanel("Environmental Analysis", mapbiomasEnvAnalysisUI(ns("envAnalysis"))),
      tabPanel("Soil", mapbiomasSoilUI(ns("soil"))),
      tabPanel("Atmosphere", mapbiomasAtmosphereUI(ns("atmosphere")))
    )
  )
}

# Server do submodulo MapBiomas - Agricultura
mapbiomasAgricultureServer <- function(id, leaflet_map, shared_geometry) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy

    rv <- reactiveValues(
      areaTimeSeriesData = NULL,
      coveragePlot = NULL,
      secondCropPlot = NULL,
      irrigationPlot = NULL,
      legendLeafItems = NULL,
      secondCropLegendItems = NULL,
      irrigationLegendItems = NULL
    )

    observeEvent(input$getAreaDistribution, {
      req(shared_geometry$userGeometry)
      geom <- if (!is.null(shared_geometry$userGeometry$source) && shared_geometry$userGeometry$source == "sf") {
        shared_geometry$userGeometry$geometry
      } else {
        leaflet_feature_to_mapbiomas_geom(shared_geometry$userGeometry)
      }
      if (is.null(geom)) {
        showNotification("Invalid geometry. Draw a polygon on the map.", type = "error")
        return()
      }

      if (is.null(rv$legendLeafItems)) {
        tryCatch({
          rv$legendLeafItems <- mapbiomas_client$get_legend_leaf_items("brazil", "default")
        }, error = function(e) {
          showNotification(paste("Error fetching MapBiomas legend:", e$message), type = "error")
          return()
        })
      }
      legend_df <- rv$legendLeafItems
      if (nrow(legend_df) == 0) {
        showNotification("MapBiomas legend is empty.", type = "error")
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
          showNotification(paste("Error fetching second crop legend (MapBiomas):", e$message), type = "error")
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
          showNotification(paste("Error fetching irrigation legend (MapBiomas):", e$message), type = "error")
          return()
        })
      }
      irrigation_legend_df <- rv$irrigationLegendItems

      years <- seq(input$yearRange[1], input$yearRange[2])
      pixel_values <- legend_df$pixelValue

      showModal(modalDialog(
        title = "Loading MapBiomas data...",
        "Querying API for each year. Please wait...",
        footer = NULL,
        size = "s"
      ))

      tryCatch({
        all_data <- list()
        second_all_data <- list()
        irrigation_all_data <- list()
        n_years <- length(years)
        withProgress(message = "Querying MapBiomas...", value = 0, {
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
          showNotification("No data returned for the period.", type = "warning")
          return()
        }

        df_all <- do.call(rbind, all_data)
        df_all <- merge(df_all, legend_df[, c("pixelValue", "name", "color")], by = "pixelValue", all.x = TRUE)
        df_all$classe <- df_all$name
        df_all$classe[is.na(df_all$classe)] <- paste0("Class ", df_all$pixelValue[is.na(df_all$classe)])

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
            title = "Area Distribution by Land Cover Class (MapBiomas)",
            xaxis = list(title = "Year"),
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
          df_second_all$classe[is.na(df_second_all$classe)] <- paste0("Class ", df_second_all$pixelValue[is.na(df_second_all$classe)])

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
              title = "Second Crop Area by Category",
              xaxis = list(title = "Year"),
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
          df_irrig_all$classe[is.na(df_irrig_all$classe)] <- paste0("Class ", df_irrig_all$pixelValue[is.na(df_irrig_all$classe)])

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
              title = "Area by Irrigation Type (MapBiomas)",
              xaxis = list(title = "Year"),
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
          title = "Time Series - MapBiomas",
          plotlyOutput(ns("areaPlot"), height = "400px"),
          tags$hr(),
          plotlyOutput(ns("secondCropPlot"), height = "400px"),
          tags$hr(),
          plotlyOutput(ns("irrigationPlot"), height = "400px"),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))

      }, error = function(e) {
        removeModal()
        showNotification(
          paste("Error fetching MapBiomas data:", e$message),
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
mapbiomasSoilServer <- function(id, leaflet_map, shared_geometry) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy

    rv_soil <- reactiveValues(
      profilePlot = NULL,
      texturePlot = NULL
    )

    # Obter ambas as mtricas (perfis + textura) e exibir donuts lado a lado
    observeEvent(input$getSoilMetrics, {
      req(shared_geometry$userGeometry)
      geom <- if (!is.null(shared_geometry$userGeometry$source) && shared_geometry$userGeometry$source == "sf") {
        shared_geometry$userGeometry$geometry
      } else {
        leaflet_feature_to_mapbiomas_geom(shared_geometry$userGeometry)
      }
      if (is.null(geom)) {
        showNotification("Invalid geometry. Draw a polygon on the map.", type = "error")
        return()
      }
      showModal(modalDialog("Loading soil metrics...", footer = NULL, size = "s"))
      tryCatch({
        # 1) Perfis (frao)
        frac_opt <- SOIL_FRACTION_OPTS[[input$soilFraction]]
        depth_profile <- input$soilProfileDepth
        legend_frac <- mapbiomas_client$get_legend_leaf_items("brazil", frac_opt$legend)
        if (nrow(legend_frac) == 0) stop("Soil profile legend not available.")
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
        df_frac$classe[is.na(df_frac$classe)] <- paste0("Class ", df_frac$pixelValue[is.na(df_frac$classe)])
        df_frac$area_ha <- round(df_frac$area_ha, 2)
        p_frac <- plot_ly(df_frac, labels = ~classe, values = ~area_ha, type = "pie", hole = 0.5,
                          marker = list(colors = df_frac$color),
                          textinfo = "none",
                          hovertemplate = "%{label}<br>%{value:.2f} ha<br>%{percent:.2f}%<extra></extra>") %>%
          layout(
            title = list(
              text = sprintf("%s - Depth %s cm", input$soilFraction, gsub("_", "-", depth_profile)),
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
        if (nrow(legend_tex) == 0) stop("Texture legend not available.")
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
        df_tex$classe[is.na(df_tex$classe)] <- paste0("Class ", df_tex$pixelValue[is.na(df_tex$classe)])
        df_tex$area_ha <- round(df_tex$area_ha, 2)
        p_tex <- plot_ly(df_tex, labels = ~classe, values = ~area_ha, type = "pie", hole = 0.5,
                         marker = list(colors = df_tex$color),
                         textinfo = "none",
                         hovertemplate = "%{label}<br>%{value:.2f} ha<br>%{percent:.2f}%<extra></extra>") %>%
          layout(
            title = list(
              text = sprintf("%s - Depth %s cm", input$soilTextureType, gsub("_", "-", depth_tex)),
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
          title = "Soil metrics - Area distribution",
          fluidRow(
            column(6, plotlyOutput(ns("soilProfilePlot"), height = "420px")),
            column(6, plotlyOutput(ns("soilTexturePlot"), height = "420px"))
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error fetching soil metrics:", e$message), type = "error")
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

# Server do submodulo MapBiomas - Environmental Analysis
mapbiomasEnvAnalysisServer <- function(id, leaflet_map, shared_geometry) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy

    rv_env <- reactiveValues(
      hipsometryPlot = NULL,
      slopePlot = NULL,
      aspectPlot = NULL
    )

    observeEvent(input$getEnvMetrics, {
      req(shared_geometry$userGeometry)
      geom <- if (!is.null(shared_geometry$userGeometry$source) && shared_geometry$userGeometry$source == "sf") {
        shared_geometry$userGeometry$geometry
      } else {
        leaflet_feature_to_mapbiomas_geom(shared_geometry$userGeometry)
      }
      if (is.null(geom)) {
        showNotification("Invalid geometry. Draw a polygon on the map.", type = "error")
        return()
      }

      showModal(modalDialog(
        title = "Loading Environmental Analysis metrics...",
        "Querying MapBiomas API (Hipsometry, Slope, Aspect). Please wait...",
        footer = NULL,
        size = "s"
      ))

      tryCatch({
        plots <- list()
        for (metric_name in names(ENV_ANALYSIS_OPTS)) {
          opt <- ENV_ANALYSIS_OPTS[[metric_name]]
          result <- mapbiomas_client$get_area_statistics(
            region = "brazil",
            subtheme_key = opt$subtheme,
            legend_key = opt$legend,
            pixel_value = opt$pixel_values,
            year = NULL,
            geometry = geom,
            spatial_method = "union"
          )
          df <- parse_area_statistics_to_df(result)
          if (nrow(df) == 0) next

          leg <- mapbiomas_client$get_legend_by_key("brazil", opt$legend)
          items <- leg$legend$items
          pv_to_name <- setNames(
            sapply(items, function(x) x$name[["en-US"]] %||% x$name[["pt-BR"]] %||% ""),
            sapply(items, function(x) x$pixelValue)
          )
          pv_to_color <- setNames(
            sapply(items, function(x) x$color %||% "#999999"),
            sapply(items, function(x) x$pixelValue)
          )

          df$label <- pv_to_name[as.character(df$pixelValue)]
          df$label[is.na(df$label) | df$label == ""] <- paste0("Class ", df$pixelValue[is.na(df$label) | df$label == ""])
          if (metric_name == "Slope") {
            range_str <- SLOPE_PERCENT_RANGES[as.character(df$pixelValue)]
            df$label <- paste0(df$label, ifelse(!is.na(range_str), paste0(" (", range_str, ")"), ""))
          }
          df$color <- pv_to_color[as.character(df$pixelValue)]
          df$color[is.na(df$color)] <- "#999999"
          df$area_ha <- round(df$area_ha, 2)

          p <- plot_ly(df, labels = ~label, values = ~area_ha, type = "pie",
                       hole = 0.6,
                       marker = list(colors = df$color),
                       textinfo = "none",
                       hovertemplate = "%{label}<br>%{value:.2f} ha<br>%{percent:.2f}%<extra></extra>") %>%
            layout(
              title = list(text = metric_name, font = list(size = 14)),
              showlegend = TRUE,
              legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
              margin = list(t = 50, b = 60),
              paper_bgcolor = "transparent",
              plot_bgcolor = "transparent"
            )
          plots[[metric_name]] <- p
        }

        rv_env$hipsometryPlot <- plots[["Hipsometry"]]
        rv_env$slopePlot <- plots[["Slope"]]
        rv_env$aspectPlot <- plots[["Aspect"]]

        removeModal()
        modal_plots <- list()
        if (!is.null(plots[["Hipsometry"]])) modal_plots <- c(modal_plots, list(column(4, plotlyOutput(ns("envHipsometryPlot"), height = "380px"))))
        if (!is.null(plots[["Slope"]])) modal_plots <- c(modal_plots, list(column(4, plotlyOutput(ns("envSlopePlot"), height = "380px"))))
        if (!is.null(plots[["Aspect"]])) modal_plots <- c(modal_plots, list(column(4, plotlyOutput(ns("envAspectPlot"), height = "380px"))))
        if (length(modal_plots) == 0) {
          removeModal()
          showNotification("No data returned for the geometry.", type = "warning")
          return()
        }
        showModal(modalDialog(
          title = "Environmental Analysis - Hipsometry, Slope and Aspect",
          fluidRow(modal_plots),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))
      }, error = function(e) {
        removeModal()
        showNotification(paste("Error fetching Environmental Analysis metrics:", e$message), type = "error")
      })
    })

    output$envHipsometryPlot <- renderPlotly({
      req(rv_env$hipsometryPlot)
      rv_env$hipsometryPlot
    })
    output$envSlopePlot <- renderPlotly({
      req(rv_env$slopePlot)
      rv_env$slopePlot
    })
    output$envAspectPlot <- renderPlotly({
      req(rv_env$aspectPlot)
      rv_env$aspectPlot
    })
  })
}

# Server do submodulo MapBiomas - Atmosphere (precipitacao)
mapbiomasAtmosphereServer <- function(id, leaflet_map, shared_geometry) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    mainInput <- leaflet_map$mapInput
    map <- leaflet_map$proxy

    rv_atm <- reactiveValues(
      precipData = NULL,
      tempData = NULL,
      precipClimatologyPlot = NULL,
      precipAnomalyPlot = NULL,
      tempClimatologyPlot = NULL,
      tempAnomalyPlot = NULL
    )

    observeEvent(input$getPrecipitationPlots, {
      req(shared_geometry$userGeometry)
      geom <- if (!is.null(shared_geometry$userGeometry$source) && shared_geometry$userGeometry$source == "sf") {
        shared_geometry$userGeometry$geometry
      } else {
        leaflet_feature_to_mapbiomas_geom(shared_geometry$userGeometry)
      }
      if (is.null(geom)) {
        showNotification("Invalid geometry. Draw a polygon on the map.", type = "error")
        return()
      }

      showModal(modalDialog(
        title = "Loading precipitation and temperature data...",
        "Querying MapBiomas API. Please wait...",
        footer = NULL,
        size = "s"
      ))

      tryCatch({
        result_precip <- mapbiomas_client$get_precipitation_statistics(
          region = "brazil", geometry = geom, years = 1985:2024
        )
        result_temp <- mapbiomas_client$get_temperature_statistics(
          region = "brazil", geometry = geom, years = 1985:2024
        )
        df_precip <- parse_precipitation_statistics_to_df(result_precip)
        df_temp <- parse_precipitation_statistics_to_df(result_temp)
        if (is.null(df_precip) || nrow(df_precip) == 0) {
          removeModal()
          showNotification("No precipitation data returned for the geometry.", type = "warning")
          return()
        }
        if (is.null(df_temp) || nrow(df_temp) == 0) {
          removeModal()
          showNotification("No temperature data returned for the geometry.", type = "warning")
          return()
        }

        rv_atm$precipData <- df_precip
        rv_atm$tempData <- df_temp

        month_names <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

        # Helper to build climatology plot
        build_climatology_plot <- function(df, title, y_label) {
          df$month_label <- factor(month_names[df$month], levels = month_names)
          clim_data <- df
          years_avail <- sort(unique(df$year))
          n_traces <- 1 + length(years_avail)
          p <- plot_ly() %>%
            add_trace(data = clim_data, x = ~month_label, y = ~value,
                      type = "box", name = "Climatology",
                      boxpoints = "outliers",
                      marker = list(color = "lightblue"),
                      line = list(color = "rgb(100,100,150)"),
                      showlegend = FALSE)
          year_visible <- rep(FALSE, length(years_avail))
          year_visible[length(years_avail)] <- TRUE
          for (i in seq_along(years_avail)) {
            yr <- years_avail[i]
            df_yr <- clim_data[clim_data$year == yr, ]
            df_yr <- df_yr[order(df_yr$month), ]
            p <- p %>% add_trace(data = df_yr, x = ~month_label, y = ~value,
                                 type = "scatter", mode = "lines+markers",
                                 name = as.character(yr),
                                 line = list(color = "red"), marker = list(size = 8),
                                 visible = year_visible[i])
          }
          buttons_list <- lapply(seq_along(years_avail), function(i) {
            vis <- rep(FALSE, n_traces)
            vis[1] <- TRUE
            vis[1 + i] <- TRUE
            list(label = as.character(years_avail[i]), method = "update",
                 args = list(list(visible = vis), list()))
          })
          p %>% layout(
            title = title,
            xaxis = list(title = "Month"),
            yaxis = list(title = y_label),
            showlegend = TRUE,
            legend = list(orientation = "v", x = 1.02, y = 1, xanchor = "left", yanchor = "top"),
            margin = list(r = 180),
            updatemenus = list(list(
              active = length(years_avail) - 1, type = "dropdown",
              buttons = buttons_list, direction = "down", showactive = TRUE,
              x = 1.02, xanchor = "left", y = 0.98, yanchor = "top"
            ))
          )
        }

        # Helper to build anomaly plot
        build_anomaly_plot <- function(df, title, y_label) {
          clim_monthly <- aggregate(value ~ month, data = df, FUN = mean, na.rm = TRUE)
          names(clim_monthly)[2] <- "climatology"
          df_anom <- merge(df, clim_monthly, by = "month")
          df_anom$anomaly <- df_anom$value - df_anom$climatology
          df_anom$date <- as.Date(paste(df_anom$year, df_anom$month, "01"), "%Y %m %d")
          df_anom <- df_anom[order(df_anom$date), ]
          n_rows <- nrow(df_anom)
          xaxis_config <- list(
            title = "Date",
            rangeslider = list(visible = TRUE, thickness = 0.05),
            rangeselector = list(buttons = list(
              list(count = 1, label = "1m", step = "month", stepmode = "backward"),
              list(count = 6, label = "6m", step = "month", stepmode = "backward"),
              list(count = 1, label = "YTD", step = "year", stepmode = "todate"),
              list(count = 1, label = "1y", step = "year", stepmode = "backward"),
              list(step = "all", label = "All")
            ))
          )
          if (n_rows >= 12) {
            last_12 <- tail(df_anom, 12)
            xaxis_config$range <- c(as.character(min(last_12$date)), as.character(max(last_12$date)))
          }
          plot_ly() %>%
            add_trace(data = df_anom, x = ~date, y = ~climatology,
                      type = "scatter", mode = "lines", name = "Climatology",
                      line = list(color = "lightgray", dash = "dot"),
                      fill = "tozeroy", fillcolor = "rgba(200,200,200,0.3)") %>%
            add_trace(data = df_anom, x = ~date, y = ~value,
                      type = "scatter", mode = "lines+markers", name = "Observed",
                      line = list(color = "green"), marker = list(size = 6)) %>%
            add_bars(data = df_anom, x = ~date, y = ~anomaly, name = "Anomaly",
                     marker = list(color = ~ifelse(anomaly >= 0, "rgba(0,100,255,0.6)", "rgba(255,50,50,0.6)"))) %>%
            layout(
              title = title,
              xaxis = xaxis_config,
              yaxis = list(title = y_label),
              showlegend = TRUE,
              legend = list(orientation = "v", x = 1.02, xanchor = "left", y = 1, yanchor = "top"),
              margin = list(b = 80, t = 50, r = 120)
            )
        }

        rv_atm$precipClimatologyPlot <- build_climatology_plot(df_precip, "Climatology - Monthly Precipitation", "mm/month")
        rv_atm$tempClimatologyPlot <- build_climatology_plot(df_temp, "Climatology - Monthly Mean Temperature", "\u00B0C")
        rv_atm$precipAnomalyPlot <- build_anomaly_plot(df_precip, "Anomaly - Precipitation vs Climatology", "mm/month")
        rv_atm$tempAnomalyPlot <- build_anomaly_plot(df_temp, "Anomaly - Temperature vs Climatology", "\u00B0C")

        removeModal()
        showModal(modalDialog(
          title = "Precipitation and Temperature - Climatology and Anomaly",
          fluidRow(
            column(6, plotlyOutput(ns("precipClimatologyPlot"), height = "320px"),
                   style = "padding-right: 12px;"),
            column(6, plotlyOutput(ns("tempClimatologyPlot"), height = "320px"),
                   style = "padding-left: 12px;")
          ),
          fluidRow(
            column(6, plotlyOutput(ns("precipAnomalyPlot"), height = "360px"),
                   style = "padding-right: 12px; margin-top: 24px;"),
            column(6, plotlyOutput(ns("tempAnomalyPlot"), height = "360px"),
                   style = "padding-left: 12px; margin-top: 24px;")
          ),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close")
        ))

      }, error = function(e) {
        removeModal()
        showNotification(
          paste("Error fetching data:", e$message),
          type = "error"
        )
      })
    })

    output$precipClimatologyPlot <- renderPlotly({
      req(rv_atm$precipClimatologyPlot)
      rv_atm$precipClimatologyPlot
    })
    output$tempClimatologyPlot <- renderPlotly({
      req(rv_atm$tempClimatologyPlot)
      rv_atm$tempClimatologyPlot
    })
    output$precipAnomalyPlot <- renderPlotly({
      req(rv_atm$precipAnomalyPlot)
      rv_atm$precipAnomalyPlot
    })
    output$tempAnomalyPlot <- renderPlotly({
      req(rv_atm$tempAnomalyPlot)
      rv_atm$tempAnomalyPlot
    })
  })
}

# Server do modulo MapBiomas (container)
mapbiomasServer <- function(id, leaflet_map, shared_geometry) {
  moduleServer(id, function(input, output, session) {
    mapbiomasAgricultureServer("agriculture", leaflet_map, shared_geometry)
    mapbiomasEnvAnalysisServer("envAnalysis", leaflet_map, shared_geometry)
    mapbiomasSoilServer("soil", leaflet_map, shared_geometry)
    mapbiomasAtmosphereServer("atmosphere", leaflet_map, shared_geometry)
  })
}