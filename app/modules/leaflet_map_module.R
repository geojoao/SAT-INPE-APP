library(shiny)
library(leaflet)
library(leaflet.extras)
library(dplyr)
library(purrr)
library(logger)
library(httr)
library(xml2)
library(htmltools)
library(htmlwidgets)
library(jsonlite)

# Lista de workspaces a serem ignorados (pode adicionar mais se necessario)
ignoredWorkspaces <- c("NASA", "INCRA")

fixedBaseGroups <- c("OpenStreetMap", "Satellite")

# Funcao para buscar as layers do GeoServer e agrupar por workspace
get_geoserver_layers <- function(
  url = "https://geoserver.bocombbm.com.br/geoserver/ows?service=WMS&version=1.3.0&request=GetCapabilities",
  ignore_workspaces = c()
) {
  tryCatch(
    {
      log_info("Buscando layers do GeoServer...")
      resp <- GET(url, config(ssl_verifypeer = FALSE, ssl_verifyhost = FALSE))
      txt <- content(resp, as = "text", encoding = "UTF-8")
      doc <- read_xml(txt)
      layer_nodes <- xml_find_all(doc, "//*[local-name()='Layer' and @queryable='1']")
      layers_list <- lapply(layer_nodes, function(node) {
        name <- xml_text(xml_find_first(node, "./*[local-name()='Name']"))
        title <- xml_text(xml_find_first(node, "./*[local-name()='Title']"))
        if (is.na(title) || title == "") title <- name
        if (is.na(name)) {
          return(NULL)
        }
        data.frame(full_name = name, title = title, stringsAsFactors = FALSE)
      })
      layers_list <- layers_list[!sapply(layers_list, is.null)]
      if (length(layers_list) == 0) {
        return(NULL)
      }
      layers_df <- bind_rows(layers_list) %>%
        mutate(
          workspace = sub(":.*", "", full_name),
          layer_name = sub(".*:", "", full_name)
        ) %>%
        filter(!workspace %in% ignore_workspaces)
      log_info(paste("Encontradas", nrow(layers_df), "layers apos filtragem."))
      return(layers_df)
    },
    error = function(e) {
      log_error(paste("Error fetching GeoServer layers:", e$message))
      return(NULL)
    }
  )
}

# Monta a arvore de overlays agrupando por workspace
build_overlay_tree <- function(layers_df) {
  workspaces <- unique(layers_df$workspace)
  tree_children <- lapply(workspaces, function(ws) {
    ws_layers <- layers_df %>% filter(workspace == ws)
    list(
      label = toupper(ws), # Workspace em CAPSLOCK
      collapsed = TRUE,
      children = lapply(1:nrow(ws_layers), function(i) {
        # Nome do layer capitalizado e _ vira espaco
        layer_title <- ws_layers$title[i]
        layer_title <- gsub("_", " ", layer_title)
        layer_title <- tools::toTitleCase(tolower(layer_title))
        list(label = layer_title, layerId = ws_layers$full_name[i])
      })
    )
  })
  list(
    label = "Camadas GeoServer",
    children = tree_children
  )
}

# Funcao para adicionar o controle de layers agrupadas (tree)
addLayersControlTree <- function(map, baseTree, overlayTree = NULL, options = list(), hiddenLayers = NULL) {
  layerTreePlugin <- htmlDependency(
    name = "Leaflet.Control.Layers.Tree",
    version = "1.1.1",
    src = c(href = "https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/"),
    script = "L.Control.Layers.Tree.js",
    stylesheet = "L.Control.Layers.Tree.css"
  )
  registerPlugin <- function(map, plugin) {
    map$dependencies <- c(map$dependencies, list(plugin))
    map
  }
  defaultOptions <- list(
    namedToggle = FALSE,
    collapseAll = "Collapse all",
    expandAll = "Expand all",
    collapsed = FALSE
  )
  options <- modifyList(defaultOptions, options)
  baseTreeJSON <- toJSON(baseTree, auto_unbox = TRUE, json_verbatim = TRUE)
  overlayTreeJSON <- if (!is.null(overlayTree)) toJSON(overlayTree, auto_unbox = TRUE, json_verbatim = TRUE) else "null"
  optionsJSON <- toJSON(options, auto_unbox = TRUE, json_verbatim = TRUE)
  hiddenLayersJSON <- if (!is.null(hiddenLayers)) toJSON(hiddenLayers, auto_unbox = TRUE) else "[]"
  jsCode <- sprintf("function(el, x) {
    var mapInstance = this;
    function getLayerById(id) {
      var found = null;
      if (mapInstance.layerManager) {
        found = mapInstance.layerManager.getLayer('image', id);
        if (!found) found = mapInstance.layerManager.getLayer('tile', id);
        if (!found) found = mapInstance.layerManager.getLayer('shape', id);
        if (!found) found = mapInstance.layerManager.getLayer('geojson', id);
        if (!found) found = mapInstance.layerManager.getLayerGroup(id, false);
      }
      if (!found) {
        mapInstance.eachLayer(function(layer) {
          if(layer.options && layer.options.layerId === id) found = layer;
          if (!found && layer.eachLayer) {
            layer.eachLayer(function(sublayer) {
              if (sublayer.options && sublayer.options.layerId === id) found = sublayer;
            });
          }
        });
      }
      return found;
    }
    function assignLayers(tree) {
      if(tree.layerId) {
        tree.layer = getLayerById(tree.layerId);
      }
      if(tree.children) {
        for(var i = 0; i < tree.children.length; i++) {
          assignLayers(tree.children[i]);
        }
      }
    }
    var baseTreeObj = %s;
    var overlayTreeObj = %s;
    assignLayers(baseTreeObj);
    if (overlayTreeObj !== null) assignLayers(overlayTreeObj);
    var ctlOptions = %s;
    var controlSpecificOptions = {
      namedToggle: ctlOptions.namedToggle,
      collapseAll: ctlOptions.collapseAll,
      expandAll: ctlOptions.expandAll,
      collapsed: ctlOptions.collapsed,
      labelIsSelector: ctlOptions.labelIsSelector,
      closedSymbol: ctlOptions.closedSymbol,
      openedSymbol: ctlOptions.openedSymbol,
      spaceSymbol: ctlOptions.spaceSymbol,
      selectorAddPoints: ctlOptions.selectorAddPoints,
      groupSelectorParent: ctlOptions.groupSelectorParent,
      position: ctlOptions.position
    };
    var ctl = L.control.layers.tree(baseTreeObj, overlayTreeObj, controlSpecificOptions);
    ctl.addTo(mapInstance).collapseTree(true).expandSelected(false);
    mapInstance._layersTreeControl = ctl;
    if (typeof window !== 'undefined') window._leafletLayersTreeControl = ctl;
    var hiddenLayers = %s;
    hiddenLayers.forEach(function(layerId) {
      var layer = getLayerById(layerId);
      if (layer) {
        mapInstance.removeLayer(layer);
      }
    });
  }", baseTreeJSON, overlayTreeJSON, optionsJSON, hiddenLayersJSON)
  onRender(map %>% registerPlugin(layerTreePlugin), jsCode)
}

# Script para handler que atualiza a arvore de overlays quando o Image Viewer adiciona camadas
# Usa setTimeout para garantir que as camadas (addRasterImage/addRasterRGB) ja foram adicionadas
# ao mapa pelo leaflet proxy antes de associar ao controle
LAYERS_TREE_UPDATE_SCRIPT <- HTML("
  Shiny.addCustomMessageHandler('updateOverlayTree', function(tree) {
    var ctl = window._leafletLayersTreeControl;
    if (!ctl || !ctl._map) return;
    var map = ctl._map;
    function getLayerById(id) {
      var found = null;
      if (map.layerManager) {
        found = map.layerManager.getLayer('image', id);
        if (!found) found = map.layerManager.getLayer('tile', id);
        if (!found) found = map.layerManager.getLayer('shape', id);
        if (!found) found = map.layerManager.getLayer('geojson', id);
        if (!found) found = map.layerManager.getLayerGroup(id, false);
      }
      if (!found) {
        map.eachLayer(function(layer) {
          if (layer.options && layer.options.layerId === id) { found = layer; }
          if (!found && layer.eachLayer) {
            layer.eachLayer(function(sublayer) {
              if (sublayer.options && sublayer.options.layerId === id) { found = sublayer; }
            });
          }
        });
      }
      return found;
    }
    function doUpdate() {
      function assignLayers(t) {
        if (t.layerId) { t.layer = getLayerById(t.layerId); }
        if (t.children) { for (var i = 0; i < t.children.length; i++) assignLayers(t.children[i]); }
      }
      assignLayers(tree);
      ctl.setOverlayTree(tree);
    }
    setTimeout(doUpdate, 150);
  });
")

# UI do modulo do mapa
leafletMapUI <- function(id) {
  ns <- NS(id)
  tagList(
    tags$head(
      tags$link(rel = "stylesheet", href = "https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/L.Control.Layers.Tree.css"),
      tags$script(src = "https://cdn.jsdelivr.net/npm/leaflet.control.layers.tree@1.1.1/L.Control.Layers.Tree.js"),
      tags$script(LAYERS_TREE_UPDATE_SCRIPT)
    ),
    leafletOutput(ns("map"), height = "100%")
  )
}

# Server do modulo do mapa
leafletMapServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    log_info("Inicializando modulo do mapa Leaflet")
    ns <- NS(id)
    # Busca as layers do GeoServer
    layers_df <- get_geoserver_layers(ignore_workspaces = ignoredWorkspaces)
    imageLayers <- reactiveVal(list())

    # Constri a rvore de overlays completa (GeoServer + Features + Image Viewer)
    build_full_overlay_tree <- function(img_layers = list()) {
      children <- list()
      if (!is.null(layers_df)) {
        children[[1]] <- build_overlay_tree(layers_df)
      }
      children[[length(children) + 1]] <- list(label = "Features", layerId = "Features")
      img_children <- lapply(img_layers, function(x) list(label = x$label, layerId = x$layerId))
      children[[length(children) + 1]] <- list(label = "Image Viewer", collapsed = FALSE, children = img_children)
      list(label = "Overlays", children = children)
    }

    overlayTree <- build_full_overlay_tree()
    baseTree <- list(
      label = "Base Maps",
      children = list(
        list(label = "OpenStreetMap", layerId = "OpenStreetMap", selected = TRUE),
        list(label = "Satellite", layerId = "Satellite")
      )
    )
    output$map <- renderLeaflet({
      log_info("Renderizando mapa Leaflet")
      m <- leaflet() %>%
        addTiles() %>%
        addProviderTiles(providers$OpenStreetMap, group = "OpenStreetMap", options = providerTileOptions(layerId = "OpenStreetMap")) %>%
        addProviderTiles(providers$Esri.WorldImagery, group = "Satellite", options = providerTileOptions(layerId = "Satellite"))
      # Adiciona as layers do GeoServer agrupadas por workspace
      if (!is.null(layers_df)) {
        for (i in 1:nrow(layers_df)) {
          # Nome do grupo exibido igual ao label do controle (capitalizado)
          group_label <- gsub("_", " ", layers_df$title[i])
          group_label <- tools::toTitleCase(tolower(group_label))
          m <- m %>% addWMSTiles(
            baseUrl = paste0("https://geoserver.bocombbm.com.br/geoserver/", layers_df$workspace[i], "/wms"),
            layers = layers_df$full_name[i],
            layerId = layers_df$full_name[i],
            options = WMSTileOptions(
              format = "image/png",
              transparent = TRUE,
              layerId = layers_df$full_name[i]
            ),
            group = group_label
          )
        }
      }
      m <- m %>%
        addDrawToolbar(
          targetGroup = "Features",
          polylineOptions = FALSE,
          circleOptions = FALSE,
          markerOptions = TRUE,
          rectangleOptions = FALSE,
          polygonOptions = TRUE,
          circleMarkerOptions = FALSE,
          editOptions = editToolbarOptions()
        ) %>%
        leaflet::addMeasure(
          position = "bottomleft",
          primaryLengthUnit = "kilometers",
          primaryAreaUnit = "hectares",
          activeColor = "#3D535D",
          completedColor = "#7D4479",
          localization = "en"
        ) %>%
        addLayersControlTree(
          baseTree = baseTree,
          overlayTree = overlayTree,
          options = list(
            collapsed = TRUE,
            position = "bottomleft",
            namedToggle = TRUE,
            selectorBack = FALSE,
            closedSymbol = "&#8862; &#x1f5c0;",
            openedSymbol = "&#8863; &#x1f5c1;"
          ),
          hiddenLayers = if (!is.null(layers_df)) layers_df$full_name else NULL
        ) %>%
        showGroup("Features") %>%
        setView(lng = -51.9253, lat = -14.2350, zoom = 4)
      m
    })
    mapProxy <- leafletProxy(ns("map"))

    # Adiciona camada do Image Viewer  rvore de controle (em vez de addLayersControl)
    addImageLayer <- function(layerId, label) {
      current <- imageLayers()
      new_layers <- c(current, list(list(layerId = layerId, label = label)))
      imageLayers(new_layers)
      tree <- build_full_overlay_tree(new_layers)
      session$sendCustomMessage("updateOverlayTree", tree)
    }

    return(list(
      proxy = mapProxy,
      mapInput = input,
      overlayGroups = if (!is.null(layers_df)) layers_df$full_name else character(0),
      baseGroups = fixedBaseGroups,
      addImageLayer = addImageLayer
    ))
  })
}
