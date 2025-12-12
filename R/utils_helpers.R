# R/utils_helpers.R

# Helper: carga segura de datos (si no existen, crea dummies)
safe_read_gpkg <- function(path, layer = NULL) {
  if (file.exists(path)) {
    tryCatch({
      st_read(path, layer = layer, quiet = TRUE) %>% st_transform(4326)
    }, error = function(e){
      message("Error leyendo ", path, ": ", e$message)
      NULL
    })
  } else {
    message("Archivo no encontrado: ", path)
    NULL
  }
}

# Funciones para isócronas / fallback

# Intentar usar osrm::osrmIsochrone si está disponible.
compute_isochrones_osrm <- function(lon, lat, breaks = c(15, 30)){
  # osrm expects loc = data.frame(id = ..., lon = ..., lat = ...)
  loc <- data.frame(id = "pt1", lon = lon, lat = lat)
  tryCatch({
    iso <- osrm::osrmIsochrone(loc = loc, breaks = breaks)
    # osrm returns an sf object with geometry and min/max minutes
    iso
  }, error = function(e){
    message("osrmIsochrone failed: ", e$message)
    NULL
  })
}

# Fallback: crear buffers por distancia estimada desde velocidad media
# Asumimos velocidad promedio en km/h (p. ej. 40 km/h por vías urbanas)
compute_isochrones_buffer <- function(lon, lat, breaks = c(15, 30), speed_kmh = 40){
  # time (min) -> distance (meters)
  times_h <- breaks / 60
  dists_m <- times_h * speed_kmh * 1000
  pt <- st_sfc(st_point(c(lon, lat)), crs = 4326)
  pt_proj <- st_transform(pt, 3857)
  polys <- lapply(dists_m, function(d) st_buffer(pt_proj, d))
  polys_sf <- st_as_sf(data.frame(min = breaks, geometry = st_sfc(polys)))
  st_set_crs(polys_sf, 3857) %>% st_transform(4326)
}
