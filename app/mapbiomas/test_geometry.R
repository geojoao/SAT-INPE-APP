# Teste do cliente MapBiomas com geometria WKT e GeoJSON
# Execute: Rscript app/mapbiomas/test_geometry.R

if (!require("logger", quietly = TRUE)) install.packages("logger", repos = "https://cloud.r-project.org", quiet = TRUE)
source("app/mapbiomas/mapbiomas_client.R")

client <- MapBiomasAPIClient$new()

cat("=== Teste 1: WKT ===\n")
wkt <- "POLYGON((-47 -23, -46.9 -23, -46.9 -22.9, -47 -22.9, -47 -23))"
result <- client$get_area_statistics(
  region = "brazil",
  subtheme_key = "coverage_lclu",
  legend_key = "default",
  pixel_value = c(1, 3, 4, 15, 18, 26),
  year = 2020,
  geometry = wkt
)
print(parse_area_statistics_to_df(result))
cat("Total:", round(result$statistic[[1]]$total), "ha\n\n")

cat("=== Teste 2: GeoJSON (lista) ===\n")
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
print(parse_area_statistics_to_df(result))
cat("Total:", round(result$statistic[[1]]$total), "ha\n\n")

cat("=== Teste 3: GeoJSON (string) ===\n")
geojson_str <- '{"type":"Polygon","coordinates":[[[-47,-23],[-46.9,-23],[-46.9,-22.9],[-47,-22.9],[-47,-23]]]}'
result <- client$get_area_statistics(
  region = "brazil",
  subtheme_key = "coverage_lclu",
  legend_key = "default",
  pixel_value = c(1, 3, 4),
  year = 2020,
  geometry = geojson_str
)
print(parse_area_statistics_to_df(result))
cat("Total:", round(result$statistic[[1]]$total), "ha\n\n")

cat("✓ Todos os testes passaram!\n")
