# global.R
# Carga de librerías y datos globales

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(plotly)
library(DT)

# Opcional: osrm para isócronas
has_osrm <- requireNamespace("osrm", quietly = TRUE)

# Nota: R/utils_helpers.R se carga automáticamente antes de global.R

# Rutas de datos
ageb_fp <- "data/processed/ageb_factibilidad.gpkg"
denue_fp <- "data/processed/denue_salud.gpkg"
egresos_fp <- "data/processed/egresos_all.csv"
delitos_fp <- "data/processed/delitos_manzanillo.csv"

# Cargar datos
ageb <- safe_read_gpkg(ageb_fp)
denue <- safe_read_gpkg(denue_fp)

if (file.exists(egresos_fp)) {
  egresos <- read_csv(egresos_fp, show_col_types = FALSE)
} else {
  egresos <- NULL
}

if (file.exists(delitos_fp)) {
  delitos <- read_csv(delitos_fp, show_col_types = FALSE)
} else {
  delitos <- NULL
}

# Si no hay ageb, crear dummy rápido (para desarrollo)
if (is.null(ageb)) {
  message("Generando datos dummy (AGEB) para desarrollo...")
  bbox <- st_as_sfc(st_bbox(c(xmin=-104.45, ymin=19.00, xmax=-104.20, ymax=19.20), crs=4326))
  ageb <- st_make_grid(bbox, n = c(6,6)) %>% st_as_sf() %>%
    mutate(CVEGEO = paste0("06007", row_number()),
           poblacion = sample(100:3000, length(.), replace = TRUE)) %>%
    st_set_geometry("geometry")
  ageb$densidad <- ageb$poblacion / as.numeric(st_area(ageb)/1e6)
  ageb$score_demanda <- scales::rescale(ageb$densidad) + runif(nrow(ageb),0,1)
}

ageb <- ageb %>% mutate(label = paste0("AGEB: ", ifelse(!is.null(.data$CVEGEO), CVEGEO, row_number()),
                                       "<br>Pob: ", ifelse(!is.null(.data$POBTOT), POBTOT, "NA")))

# si denue está vacío, crear dummies
if (is.null(denue)) {
  message("Generando puntos DENUE dummy...")
  pts <- st_sample(ageb, size = 300)
  denue <- st_as_sf(data.frame(pt = 1:length(pts)), geometry = unlist(pts))
  denue$Nombre <- paste0("Estab ", seq_len(nrow(denue)))
  denue$Clase_actividad <- sample(c("Farmacia","Clínica","Consultorio","Laboratorio"), nrow(denue), replace = TRUE)
}

# Si egresos es NULL, generar dummies
if (is.null(egresos)) {
  message("Generando datos dummy (Egresos)...")
  bounds <- st_bbox(ageb)
  egresos <- data.frame(
    CLUES = paste0("CLUES_DUMMY_", 1:50),
    longitud = runif(50, bounds["xmin"], bounds["xmax"]),
    latitud = runif(50, bounds["ymin"], bounds["ymax"]),
    TOTAL_EGRESOS = sample(10:1000, 50, replace = TRUE),
    ano = sample(2018:2022, 50, replace = TRUE)
  )
}
