# MapBiomas API Client para R

Cliente R para a [API do MapBiomas](https://prd.plataforma.mapbiomas.org/api/docs#/), permitindo extrair estatísticas de área, distribuição de classes de cobertura, transições, histórico de pixels e outras funcionalidades.

## Instalação das dependências

```r
install.packages(c("httr", "jsonlite", "R6", "logger"))
```

## Uso básico

```r
source("mapbiomas/mapbiomas_client.R")

# Inicializar cliente
client <- MapBiomasAPIClient$new()

# Listar regiões disponíveis
regions <- client$get_regions()

# Distribuição de área por classes para um território (ex: Brasil)
# subthemeKey: coverage_lclu = cobertura de uso e cobertura do solo
# legendKey: default = legenda padrão de classes
# pixelValue: códigos das classes (1=Floresta, 3=Formação Florestal, 4=Formação Savânica, etc)
result <- client$get_area_statistics(
  region = "brazil",
  subtheme_key = "coverage_lclu",
  legend_key = "default",
  pixel_value = c(1, 3, 4, 5, 9, 11, 12, 13, 15),
  year = 2020,
  territory_id = "1-1-1"  # Brasil
)

# Converter para data frame
df <- parse_area_statistics_to_df(result)
print(df)
```

## Geometria customizada (WKT ou GeoJSON)

O cliente aceita geometria diretamente em **WKT** ou **GeoJSON**:

```r
# Com WKT (POLYGON)
wkt <- "POLYGON((-47 -23, -46.9 -23, -46.9 -22.9, -47 -22.9, -47 -23))"
result <- client$get_area_statistics(
  region = "brazil",
  subtheme_key = "coverage_lclu",
  legend_key = "default",
  pixel_value = c(1, 3, 4, 15, 18, 26),
  year = 2020,
  geometry = wkt
)

# Com GeoJSON
geojson <- list(
  type = "Polygon",
  coordinates = list(list(
    c(-47, -23), c(-46.9, -23), c(-46.9, -22.9), c(-47, -22.9), c(-47, -23)
  ))
)
result <- client$get_area_statistics(
  region = "brazil",
  subtheme_key = "coverage_lclu",
  legend_key = "default",
  pixel_value = c(1, 3, 4, 15, 18),
  year = 2020,
  geometry = geojson
)
```

**Limite:** geometria customizada deve ter área < 1.000.000 ha (~100.000 km²).

## Endpoints disponíveis

### Regions
- `get_regions()` - Lista regiões (Brazil, Chaco, Indonesia, etc)

### Territories
- `get_territories()`, `get_territory_groups()`, `get_territory_categories()`
- `get_territory_bounds()`, `get_territory_geometry()`
- `search_territories()`, `get_territory_by_point()`, `get_territory()`
- `upload_territory()` - Upload de geometria customizada
- `get_territory_tiles()` - Tiles vetoriais

### Themes e Legends
- `get_themes()`, `get_subthemes()`, `get_theme_by_id()`, `get_theme_by_key()`
- `get_legends()`, `get_legend_by_id()`, `get_legend_by_key()`
- `search_legend_items()`, `get_legend_item_by_pixel()`

### Statistics
- `get_area_statistics()` - **Distribuição de área por classes**
- `get_transitions_area()` - Transições (retorna taskID assíncrono)
- `get_statistics_task()`, `wait_for_statistics_task()` - Polling de tasks
- `get_ranking_area()`, `get_relative_area()`, `get_soil_organic_carbon()`
- `get_subtheme_statistics()`, `get_ranking_subtheme()`

### Maps
- `get_pixel_history()` - Histórico de classes em um ponto (lat/lng)
- `get_map_gif()`, `get_map_url()`, `get_map_image_url()`
- `get_soil_composition()`, `get_territory_layer()`, `get_relief_map()`
- `get_landsat_mosaic()`, `get_sankey_map()`, `export_map()`

### Properties (CAR - Brasil)
- `get_property_by_point()`, `get_property_by_code()`

### Layers
- `get_layer_types()`, `get_layer_categories()`, `get_layer_tiles()`

## Classes de cobertura (pixelValue) - Cobertura 30m

Alguns códigos comuns da legenda `default` (coverage_lclu):
- 1: Floresta
- 3: Formação Florestal
- 4: Formação Savânica
- 5: Mangue
- 9: Floresta Plantada
- 11: Campo Alagado e Área Pantanosa
- 12: Formação Não Florestal
- 13: Outras Formações Não Florestais
- 15: Pastagem
- 18: Agricultura
- 19: Lavoura Temporária
- 20: Cana
- 21: Mosaico de Usos
- 22: Área não Vegetada
- 23: Praia, Duna e Areal
- 24: Infraestrutura Urbana
- 25: Outras Áreas não Vegetadas
- 26: Corpo d'Água
- 29: Não Observado

Use `search_legend_items("floresta")` para buscar códigos por nome.
