# MapBiomas API Client for R
# Documentacao: https://prd.plataforma.mapbiomas.org/api/docs#/
#
# API v1.0 - OAS 3.0
# Permite extrair estatisticas de area, distribuicao de classes, transicoes,
# historico de pixels e outras funcionalidades do MapBiomas.
# Suporta geometria via WKT, GeoJSON ou territory_id.

library(httr)
library(jsonlite)
library(R6)
library(logger)
library(sf)

# Helper function to log request details (evita { } para nao conflitar com logger/glue)
log_request_details <- function(method, url, params = NULL, payload = NULL) {
  log_info(paste0("MapBiomas API Request - ", method, " ", url))
}

# Converte WKT ou GeoJSON para formato de coordenadas da API MapBiomas
# API espera: [[[lng,lat],[lng,lat],...]] (Polygon coordinates)
# Limite: geometria deve ter area < 1.000.000 ha
geometry_to_api_coords <- function(geometry) {
  if (is.character(geometry)) {
    geom_str <- trimws(geometry)
    if (grepl("^\\s*\\{", geom_str)) {
      geojson <- jsonlite::fromJSON(geom_str, simplifyVector = FALSE)
      return(geometry_to_api_coords(geojson))
    }
    wkt <- geom_str
    if (grepl("^POLYGON\\s*\\(", wkt, ignore.case = TRUE)) {
      wkt <- gsub("^POLYGON\\s*\\(\\s*\\(|\\)\\s*\\)\\s*$", "", wkt, ignore.case = TRUE)
      coords <- strsplit(wkt, "\\s*,\\s*")[[1]]
      ring <- lapply(coords, function(c) {
        parts <- strsplit(trimws(c), "\\s+")[[1]]
        as.numeric(parts[1:2])
      })
      return(list(ring))
    }
    geom_sf <- st_as_sfc(wkt, crs = 4326)
    geom_sf <- st_transform(geom_sf, 4326)
    if (inherits(geom_sf[[1]], "POLYGON")) {
      m <- st_coordinates(geom_sf[[1]])
      ring <- lapply(seq_len(nrow(m)), function(i) as.numeric(m[i, 1:2]))
      return(list(ring))
    }
    if (inherits(geom_sf[[1]], "MULTIPOLYGON")) {
      geom_union <- st_union(st_sf(geom = geom_sf))
      m <- st_coordinates(geom_union[[1]])
      ring <- lapply(seq_len(nrow(m)), function(i) as.numeric(m[i, 1:2]))
      return(list(ring))
    }
    stop("WKT deve ser POLYGON ou MULTIPOLYGON")
  }
  if (is.list(geometry)) {
    if (!is.null(geometry$coordinates)) {
      if (geometry$type == "Polygon") return(geometry$coordinates)
      if (geometry$type == "MultiPolygon") return(geometry$coordinates[[1]])
      if (geometry$type == "Feature") return(geometry_to_api_coords(geometry$geometry))
    }
    stop("GeoJSON invalido: deve ter type e coordinates")
  }
  if (inherits(geometry, "sf") || inherits(geometry, "sfc")) {
    geom_sf <- st_transform(st_sfc(st_geometry(geometry)[[1]]), 4326)
    if (inherits(geom_sf[[1]], "POLYGON")) {
      m <- st_coordinates(geom_sf[[1]])
      ring <- lapply(seq_len(nrow(m)), function(i) as.numeric(m[i, 1:2]))
      return(list(ring))
    }
    if (inherits(geom_sf[[1]], "MULTIPOLYGON")) {
      geom_union <- st_union(st_sf(geom = geom_sf))
      m <- st_coordinates(geom_union[[1]])
      ring <- lapply(seq_len(nrow(m)), function(i) as.numeric(m[i, 1:2]))
      return(list(ring))
    }
    stop("Objeto sf deve ser POLYGON ou MULTIPOLYGON")
  }
  stop("geometry deve ser: string WKT, lista GeoJSON ou objeto sf")
}

# Define the MapBiomasAPIClient
MapBiomasAPIClient <- R6::R6Class(
  "MapBiomasAPIClient",
  public = list(
    base_url = NULL,

    # Initialize with MapBiomas API base URL
    initialize = function(base_url = "https://prd.plataforma.mapbiomas.org/api/v1") {
      self$base_url <- base_url
      log_info(sprintf("MapBiomas API Client initialized with base URL: %s", base_url))
    },

    # ========== REGIONS ==========

    # GET /regions - List regions
    get_regions = function() {
      url <- paste0(self$base_url, "/regions")
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        log_info("Successfully retrieved regions")
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get regions - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # ========== TERRITORIES ==========

    # GET /{region}/territories - List territories
    get_territories = function(region, page = 1, limit = 100) {
      url <- paste0(self$base_url, "/", region, "/territories")
      log_request_details("GET", url, list(page = page, limit = limit))
      response <- GET(url, query = list(page = page, limit = limit))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get territories - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/territories/groups - List territory groups
    get_territory_groups = function(region) {
      url <- paste0(self$base_url, "/", region, "/territories/groups")
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get territory groups - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/territories/categories - List territory categories
    get_territory_categories = function(region) {
      url <- paste0(self$base_url, "/", region, "/territories/categories")
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get territory categories - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/territories/bounds - Get territories bounding box
    get_territory_bounds = function(region, territory_id = NULL) {
      url <- paste0(self$base_url, "/", region, "/territories/bounds")
      params <- if (!is.null(territory_id)) list(territoryId = territory_id) else list()
      log_request_details("GET", url, params)
      response <- GET(url, query = params)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get territory bounds - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/territories/geometry - Get territories geometry
    get_territory_geometry = function(region, territory_id = NULL) {
      url <- paste0(self$base_url, "/", region, "/territories/geometry")
      params <- if (!is.null(territory_id)) list(territoryId = territory_id) else list()
      log_request_details("GET", url, params)
      response <- GET(url, query = params)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get territory geometry - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/territories/search/{searchTerm} - Search territories by name
    search_territories = function(region, search_term) {
      url <- paste0(self$base_url, "/", region, "/territories/search/", URLencode(search_term, reserved = TRUE))
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to search territories - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/territories/point - Find territory by coordinates
    get_territory_by_point = function(region, longitude, latitude) {
      url <- paste0(self$base_url, "/", region, "/territories/point")
      log_request_details("GET", url, list(longitude = longitude, latitude = latitude))
      response <- GET(url, query = list(longitude = longitude, latitude = latitude))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get territory by point - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/territories/{id} - Get territory by ID
    get_territory = function(region, territory_id) {
      url <- paste0(self$base_url, "/", region, "/territories/", territory_id)
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get territory - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # POST /{region}/territories/upload - Save territory (upload custom geometry)
    upload_territory = function(region, payload) {
      url <- paste0(self$base_url, "/", region, "/territories/upload")
      log_request_details("POST", url, payload = payload)
      response <- POST(url, body = payload, encode = "json",
                       add_headers(`Content-Type` = "application/json"))
      if (response$status_code %in% c(200, 201)) {
        log_info("Successfully uploaded territory")
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to upload territory - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # ========== THEMES ==========

    # GET /{region}/themes - List themes
    get_themes = function(region, page = 1, limit = 100) {
      url <- paste0(self$base_url, "/", region, "/themes")
      log_request_details("GET", url, list(page = page, limit = limit))
      response <- GET(url, query = list(page = page, limit = limit))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get themes - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/themes/subthemes - List subthemes
    get_subthemes = function(region, page = 1, limit = 100) {
      url <- paste0(self$base_url, "/", region, "/themes/subthemes")
      log_request_details("GET", url, list(page = page, limit = limit))
      response <- GET(url, query = list(page = page, limit = limit))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get subthemes - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/themes/{id} - Get theme by ID
    get_theme_by_id = function(region, theme_id) {
      url <- paste0(self$base_url, "/", region, "/themes/", theme_id)
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get theme - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/themes/by/key/{key} - Get theme by Key
    get_theme_by_key = function(region, key) {
      url <- paste0(self$base_url, "/", region, "/themes/by/key/", URLencode(key, reserved = TRUE))
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get theme by key - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # ========== LEGENDS ==========

    # GET /{region}/legends - List legends
    get_legends = function(region, page = 1, limit = 100) {
      url <- paste0(self$base_url, "/", region, "/legends")
      log_request_details("GET", url, list(page = page, limit = limit))
      response <- GET(url, query = list(page = page, limit = limit))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get legends - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/legends/{id} - Get legend by ID
    get_legend_by_id = function(region, legend_id) {
      url <- paste0(self$base_url, "/", region, "/legends/", legend_id)
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get legend - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/legends/by/key/{key} - Get legend by Key
    get_legend_by_key = function(region, key) {
      url <- paste0(self$base_url, "/", region, "/legends/by/key/", URLencode(key, reserved = TRUE))
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get legend by key - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/legends/items/{searchTerm} - Search legend items
    search_legend_items = function(region, search_term) {
      url <- paste0(self$base_url, "/", region, "/legends/items/", URLencode(search_term, reserved = TRUE))
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to search legend items - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/legends/by/key/{key}/item/{pixelValue} - Get legend item by pixel value
    get_legend_item_by_pixel = function(region, legend_key, pixel_value) {
      url <- paste0(self$base_url, "/", region, "/legends/by/key/",
                   URLencode(legend_key, reserved = TRUE), "/item/", pixel_value)
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get legend item - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # Retorna itens folha da legenda (sem filhos) para evitar dupla contagem de area
    # region, legend_key: parametros da API; retorna data.frame com pixelValue, name, color
    get_legend_leaf_items = function(region = "brazil", legend_key = "default") {
      leg <- self$get_legend_by_key(region, legend_key)
      get_legend_leaf_items_from_response(leg)
    },

    # ========== PROPERTIES (Brazil CAR) ==========

    # GET /brazil/properties/point - Find CAR properties by coordinates
    get_property_by_point = function(longitude, latitude) {
      url <- paste0(self$base_url, "/brazil/properties/point")
      log_request_details("GET", url, list(longitude = longitude, latitude = latitude))
      response <- GET(url, query = list(longitude = longitude, latitude = latitude))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get property by point - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /brazil/properties/{code} - Find CAR property by code
    get_property_by_code = function(code) {
      url <- paste0(self$base_url, "/brazil/properties/", URLencode(code, reserved = TRUE))
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get property by code - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # ========== LAYERS ==========

    # GET /{region}/layers/types - List Layer types
    get_layer_types = function(region) {
      url <- paste0(self$base_url, "/", region, "/layers/types")
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get layer types - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/layers/{layerType}/categories - List Layers categories
    get_layer_categories = function(region, layer_type) {
      url <- paste0(self$base_url, "/", region, "/layers/", layer_type, "/categories")
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get layer categories - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/layers/tiles/{z}/{x}/{y}.mvt - Retrieve vector tiles of layer features
    get_layer_tiles = function(region, z, x, y, ...) {
      url <- paste0(self$base_url, "/", region, "/layers/tiles/", z, "/", x, "/", y, ".mvt")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(response$content)
      } else {
        log_error(sprintf("Failed to get layer tiles - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/territories/tiles/{z}/{x}/{y}.mvt - Retrieve vector tiles of territories
    get_territory_tiles = function(region, z, x, y, ...) {
      url <- paste0(self$base_url, "/", region, "/territories/tiles/", z, "/", x, "/", y, ".mvt")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(response$content)
      } else {
        log_error(sprintf("Failed to get territory tiles - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # ========== MAPS ==========

    # GET /{region}/maps/gif - Get gif
    get_map_gif = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/maps/gif")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get map gif - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/maps/pixel-history - Get pixel history (historico de classes em um ponto)
    get_pixel_history = function(region, longitude, latitude,
                                  subtheme_key, legend_key, pixel_value,
                                  territory_id = NULL) {
      url <- paste0(self$base_url, "/", region, "/maps/pixel-history")
      params <- list(
        longitude = as.numeric(longitude),
        latitude = as.numeric(latitude),
        subthemeKey = subtheme_key,
        legendKey = legend_key,
        pixelValue = as.integer(pixel_value),
        territoryId = territory_id
      )
      params <- params[!sapply(params, is.null)]
      log_request_details("GET", url, params)
      response <- GET(url, query = params)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get pixel history - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/maps/soil-composition - Get soil composition
    get_soil_composition = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/maps/soil-composition")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get soil composition - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/maps/map - Get map URL
    get_map_url = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/maps/map")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get map URL - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/maps/image.png - Get map image
    get_map_image_url = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/maps/image.png")
      params <- list(...)
      query_str <- paste0(names(params), "=", params, collapse = "&")
      return(paste0(url, "?", query_str))
    },

    # GET /{region}/maps/territory-layer - Get territory layer
    get_territory_layer = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/maps/territory-layer")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get territory layer - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/maps/relief - Get relief map URL
    get_relief_map = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/maps/relief")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get relief map - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/maps/landsat-mosaic - Get landsat mosaic map URL
    get_landsat_mosaic = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/maps/landsat-mosaic")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get landsat mosaic - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/maps/sankey - Get Sankey map URL
    get_sankey_map = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/maps/sankey")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get sankey map - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # POST /{region}/maps/export - Export map as GeoTIFF
    export_map = function(region, payload) {
      url <- paste0(self$base_url, "/", region, "/maps/export")
      log_request_details("POST", url, payload = payload)
      response <- POST(url, body = payload, encode = "json",
                       add_headers(`Content-Type` = "application/json"))
      if (response$status_code %in% c(200, 201)) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to export map - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # ========== STATISTICS ==========

    # GET /{region}/statistics/area - Get area statistics (distribuicao de area por classes)
    # Parametros: subthemeKey, legendKey, pixelValue (array), year
    # E um de: territory_id OU geometry (WKT, GeoJSON ou objeto sf)
    # Limite: geometria customizada deve ter area < 1.000.000 ha
    get_area_statistics = function(region, subtheme_key, legend_key, pixel_value,
                                   year = NULL, territory_id = NULL, geometry = NULL,
                                   filters = NULL) {
      if (is.null(territory_id) && is.null(geometry)) {
        stop("Forneca territory_id ou geometry (WKT/GeoJSON)")
      }
      if (!is.null(territory_id) && !is.null(geometry)) {
        stop("Forneca apenas territory_id OU geometry, nao ambos")
      }
      url <- paste0(self$base_url, "/", region, "/statistics/area")
      pixel_part <- paste(paste0("pixelValue=", as.integer(pixel_value)), collapse = "&")
      q_str <- paste0(
        "subthemeKey=", URLencode(subtheme_key),
        "&legendKey=", URLencode(legend_key)
      )
      if (!is.null(year)) {
        q_str <- paste0(q_str, "&year=", year)
      }
      q_str <- paste0(q_str, "&", pixel_part)
      if (!is.null(territory_id)) {
        q_str <- paste0(q_str, "&territoryId=", URLencode(territory_id))
      } else {
        coords <- geometry_to_api_coords(geometry)
        geom_json <- jsonlite::toJSON(coords, auto_unbox = TRUE)
        q_str <- paste0(q_str, "&geometry=", URLencode(geom_json, reserved = TRUE))
      }
      if (!is.null(filters)) {
        filters_json <- jsonlite::toJSON(filters, auto_unbox = TRUE)
        q_str <- paste0(q_str, "&filters=", URLencode(filters_json, reserved = TRUE))
      }
      full_url <- paste0(url, "?", q_str)
      log_request_details("GET", full_url)
      response <- GET(full_url)
      if (response$status_code == 200) {
        log_info("Successfully retrieved area statistics")
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get area statistics - Status: %d", response$status_code))
        stop("Error: ", response$status_code, " - ", content(response, "text"))
      }
    },

    # GET /{region}/statistics/transitions/area - Get transitions area statistics
    # Retorna taskID - use get_statistics_task para obter resultado
    get_transitions_area = function(region, subtheme_key, legend_key, year_interval,
                                    territory_id = NULL) {
      url <- paste0(self$base_url, "/", region, "/statistics/transitions/area")
      params <- list(
        subthemeKey = subtheme_key,
        legendKey = legend_key,
        yearInterval = year_interval,
        territoryId = territory_id
      )
      params <- params[!sapply(params, is.null)]
      log_request_details("GET", url, params)
      response <- GET(url, query = params)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get transitions - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/statistics/ranking/area - Get ranking area statistics
    get_ranking_area = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/statistics/ranking/area")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get ranking area - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/statistics/relative-area - Get relative area statistics
    get_relative_area = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/statistics/relative-area")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get relative area - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/statistics/soil-organic-carbon - Get soil organic carbon statistics
    get_soil_organic_carbon = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/statistics/soil-organic-carbon")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get soil organic carbon - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/statistics/subtheme - Get subtheme statistics
    get_subtheme_statistics = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/statistics/subtheme")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get subtheme statistics - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/statistics/ranking/subtheme - Get ranking subtheme statistics
    get_ranking_subtheme = function(region, ...) {
      url <- paste0(self$base_url, "/", region, "/statistics/ranking/subtheme")
      log_request_details("GET", url, list(...))
      response <- GET(url, query = list(...))
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get ranking subtheme - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # GET /{region}/statistics/task/{id} - Check statistics task by id (para transicoes assincronas)
    get_statistics_task = function(region, task_id) {
      url <- paste0(self$base_url, "/", region, "/statistics/task/", task_id)
      log_request_details("GET", url)
      response <- GET(url)
      if (response$status_code == 200) {
        return(content(response, as = "parsed"))
      } else {
        log_error(sprintf("Failed to get statistics task - Status: %d", response$status_code))
        stop("Error: ", response$status_code)
      }
    },

    # Helper: Aguarda task de estatisticas completar (polling)
    wait_for_statistics_task = function(region, task_id, max_wait_sec = 300, poll_interval_sec = 5) {
      elapsed <- 0
      while (elapsed < max_wait_sec) {
        result <- self$get_statistics_task(region, task_id)
        if (result$status == "completed") {
          return(result)
        }
        if (result$status == "failed") {
          stop("Statistics task failed")
        }
        Sys.sleep(poll_interval_sec)
        elapsed <- elapsed + poll_interval_sec
      }
      stop("Timeout waiting for statistics task")
    },

    # ========== HELPER: Distribuicao de area por classes para geometria ==========
    # Para geometria customizada: 1) use territoryId de um territorio existente, ou
    # 2) upload via upload_territory e use o ID retornado
    # Exemplo: coverage_lclu + legend default = classes de cobertura (1=Forest, 3=Forest Formation, etc)

    get_area_distribution = function(region = "brazil", territory_id = NULL, geometry = NULL,
                                     subtheme_key = "coverage_lclu", legend_key = "default",
                                     pixel_values = c(1, 3, 4, 5, 9, 11, 12, 13, 15, 18, 19, 20, 21, 22, 23, 24, 25, 26, 29, 30, 31, 32, 33, 39, 40, 41, 46, 47, 48, 49),
                                     year = 2020) {
      result <- self$get_area_statistics(
        region = region,
        subtheme_key = subtheme_key,
        legend_key = legend_key,
        pixel_value = pixel_values,
        year = year,
        territory_id = territory_id,
        geometry = geometry
      )
      return(result)
    }
  )
)

# Helper: Extrai itens folha da legenda (sem filhos) para evitar dupla contagem de area
get_legend_leaf_items_from_response <- function(legend_response) {
  `%||%` <- function(x, y) if (is.null(x)) y else x
  items <- legend_response$legend$items
  if (is.null(items) || length(items) == 0) {
    return(data.frame(pixelValue = integer(), name = character(), color = character(), stringsAsFactors = FALSE))
  }
  collect_leaves <- function(item_list) {
    leaves <- list()
    for (i in seq_along(item_list)) {
      item <- item_list[[i]]
      children <- item$children
      if (is.null(children) || length(children) == 0) {
        nm <- item$name[["pt-BR"]] %||% item$name[["en-US"]] %||% as.character(item$pixelValue)
        leaves[[length(leaves) + 1]] <- list(
          pixelValue = as.integer(item$pixelValue),
          name = nm,
          color = item$color %||% "#999999"
        )
      } else {
        leaves <- c(leaves, collect_leaves(children))
      }
    }
    leaves
  }
  result <- collect_leaves(items)
  if (length(result) == 0) {
    return(data.frame(pixelValue = integer(), name = character(), color = character(), stringsAsFactors = FALSE))
  }
  data.frame(
    pixelValue = sapply(result, function(x) x$pixelValue),
    name = sapply(result, function(x) x$name),
    color = sapply(result, function(x) x$color),
    stringsAsFactors = FALSE
  )
}

# Helper: Parse area statistics to data frame
parse_area_statistics_to_df <- function(api_response) {
  if (is.null(api_response$statistic) || length(api_response$statistic) == 0) {
    return(data.frame())
  }
  stat <- api_response$statistic[[1]]
  year <- if (!is.null(stat$year)) stat$year[[1]] else NA
  items <- stat$items
  if (is.null(items) || length(items) == 0) {
    return(data.frame())
  }
  df <- data.frame(
    pixelValue = sapply(items, function(x) x$pixelValue),
    area_ha = sapply(items, function(x) x$value),
    year = year,
    stringsAsFactors = FALSE
  )
  return(df)
}

# verify if the file is the main file
if (sys.nframe() == 0) {
  client <- MapBiomasAPIClient$new()
  # Exemplo: distribuicao de area para Brasil (territoryId 1-1-1)
  # result <- client$get_area_statistics("brazil", "coverage_lclu", "default", c(1,3,4), 2020, "1-1-1")
  # df <- parse_area_statistics_to_df(result)
}
