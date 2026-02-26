# GDAL: evita erro de certificado SSL no Windows (CERT_TRUST_IS_UNTRUSTED_ROOT)
# ao acessar rasters remotos via /vsicurl/ (data.inpe.br)
Sys.setenv(GDAL_HTTP_UNSAFESSL = "YES")

library(shiny)
library(plotly)
library(leaflet)
library(leaflet.extras)
library(leaflet.extras2)
library(jsonlite)
library(httr)
library(httr2)
library(base64enc)
library(R6)
library(shinyjs)
library(shinydashboard)
library(logger)  # Add logging library
library(rstac)
library(terra)
library(sf)
library(leafem)
library(raster)
library(purrr)  # Adicionada esta linha
library(BBMQuant)
library(signal)

# Source modules
source("wtss/wtss_client.R")

# Source other modules
source("modules/startup_modal_module.R")
source("modules/time_series_module.R")
source("modules/image_viewer_module.R")
source("modules/mapbiomas_module.R")
source("modules/leaflet_map_module.R")

log_threshold(TRACE)
log_appender(appender_file(stderr()))

# UI Definition
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$style(HTML("
      .center-heading {
        text-align: center;
      }
      .sidebar { 
        height: calc(100vh - 20px);
        overflow-y: auto;
        padding: 15px;
        background-color: #f8f9fa;
        position: fixed;
        width: 300px;
        left: 15px;
        top: 10px;
        border-radius: 5px;
        box-shadow: 0 0 10px rgba(0,0,0,0.1);
      }
      .map-container {
        height: 100vh;
        margin-left: 330px;
        padding: 0;
      }
      /* Modal de incio: tamanho reduzido */
      .modal-dialog.modal-sm { 
        max-width: 420px; 
        width: 90%; 
        margin: 5vh auto;
      }
      /* Modais de resultados: grandes, centralizados, altura conforme contedo */
      .modal-dialog { 
        max-width: 90%; 
        width: 90%; 
        margin: 5vh auto;
      }
      .modal-lg { 
        max-width: 92%; 
        width: 92%; 
        margin: 5vh auto;
      }
      .modal-body {
        max-height: none;
      }
      .modal-content {
        height: auto;
      }
    "))
  ),
  div(class = "sidebar",
      tags$img(src = "logo_h.png", width = "100%"),
      h2(class = "center-heading", "S.A.T."),
      tabsetPanel(id = "mainTabs",
                  tabPanel("Time Series", 
                           timeSeriesUI("timeSeries")),
                  tabPanel("Image Viewer", 
                           imageViewerUI("imageViewer")),
                  tabPanel("MapBiomas", 
                           mapbiomasUI("mapbiomas"))
      ),
      tags$a(href = "https://rstudio.bocombbm.com.br/app/quant_agro_doc/apps/sat.html", 
             h4("Help (?)"), target = "_blank")
  ),
  div(class = "map-container",
      # Use the Leaflet map module UI
      leafletMapUI("leafletMap")
  )
)

# Server Definition
server <- function(input, output, session) {
  # Log application start
  log_info("SAT application started")
  
  startupModalServer("startupModal")
  
  # Initialize Leaflet map module
  leaflet_map <- leafletMapServer("leafletMap")
  
  # Initialize other modules with access to the Leaflet map proxy and bounds
  timeSeriesServer("timeSeries", 
                   leaflet_map = leaflet_map
  )
  
  imageViewerServer("imageViewer",
                    leaflet_map = leaflet_map
  )

  mapbiomasServer("mapbiomas",
                  leaflet_map = leaflet_map
  )
  
  # Log when session ends
  session$onSessionEnded(function() {
    log_info("User session ended")
  })
}

# Run the application locally
#shinyApp(ui = ui, server = server, options = list(host = "0.0.0.0", port = 8888))

ShinyAppBBM(
  ui = ui,
  server = server,
  tenant = "44d572a6-0370-4d7f-a52c-5c3616252aac",
  app_id = "#{app_id}#",
  app_secret = "#{app_secret}#",
  resource = c("openid"),
  redirect = "https://rstudio.bocombbm.com.br/app/App_sat",
  grantedUsers = c(
    "gabrielvasconcelos@bocombbm.com.br",
    "danielreis@bocombbm.com.br",
    "reneroliveira@bocombbm.com.br",
    "joaoluizneto@bocombbm.com.br",
    "zuilhosegundo@bocombbm.com.br",
    "brunasantos@bocombbm.com.br",
    "thalescosta@bocombbm.com.br",
    "octaviorodrigues@bocombbm.com.br",
    "vitojarque@bocombbm.com.br",
    "gustavolemos@bocombbm.com.br",
    "guilhermedurante@bocombbm.com.br",
    "milenaravanini@bancobbm.onmicrosoft.com",
    "murilosouza@bocombbm.com.br",
    "joaopguimaraes@bocombbm.com.br",
    "edgardguitton@bocombbm.com.br",
    "lizrabelo@bocombbm.com.br",
    "lucassilveira@bocombbm.com.br",
    "icaroramires@bocombbm.com.br",
    "camilalosano@bocombbm.com.br",
    "joelevinson@bocombbm.com.br",
    "marciuscesar@bocombbm.com.br",
    "Joesiqueira@bocombbm.com.br",
    "lucasfavaro@bocombbm.com.br",
    "luizevora@bocombbm.com.br",
    "victorbreves@bocombbm.com.br",
    "matheusvarela@bancobbm.onmicrosoft.com",
    "felipevianna@bocombbm.com.br",
    "joaostrauss@bocombbm.com.br",
    "camilacaleones@bocombbm.com.br",
    "renanmiguel@bocombbm.com.br",
    "mariapavan@bocombbm.com.br",
    "isabelafarina@bocombbm.com.br",
    "amandabeckers@bocombbm.com.br",
    "eduardomello@bocombbm.com.br",
    "gustavocateb@bocombbm.com.br",
    "brunoleitao@bocombbm.com.br",
    "marcosjunqueira@bocombbm.com.br",
    "luizfortes@bocombbm.com.br",
    "mateusamaro@bocombbm.com.br",
    "guilhermedurante@bocombbm.com.br",
    "vitorlagemann@bocombbm.com.br",
    "leonardooliveira@bocombbm.com.br",
    "vitorbossolani@bocombbm.com.br",
    "rafaelmonteiro@bocombbm.com.br",
    "fabioabbondi@bocombbm.com.br",
    "henriquesilvestre@bocombbm.com.br",
    "BrenoCampos@bocombbm.com.br",
    "Brenocampos@bocombbm.com.br",
    "brenoCampos@bocombbm.com.br",
    "brenocampos@bocombbm.com.br",
    "gabrielmattos@bocombbm.com.br",
    "guilhermemazzoni@bocombbm.com.br",
    "ramiromonarcha@bocombbm.com.br",
    "renanmiguel@bocombbm.com.br"
  ),authEnabled = TRUE#, port=8888  # Set to FALSE to disable authentication for testing  
)