FROM rocker/geospatial:4.3.1

# Instala dependências do sistema
RUN apt-get update && apt-get install -y \
    libgdal-dev \
    libproj-dev \
    libgeos-dev \
    libudunits2-dev \
    libv8-dev \
    libprotobuf-dev \
    protobuf-compiler \
    libjq-dev \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Instala pacotes R necessários
RUN install2.r --error \
    shiny \
    plotly \
    leaflet \
    leaflet.extras \
    leaflet.extras2 \
    jsonlite \
    httr \
    httr2 \
    base64enc \
    R6 \
    shinyjs \
    shinydashboard \
    logger \
    rstac \
    terra \
    sf \
    leafem \
    raster

# Expõe a porta do Shiny
EXPOSE 3838

# Define diretório de trabalho
WORKDIR /workspace 