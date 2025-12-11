# app.R
# Shiny app: Mapa interactivo de factibilidad de hospital - Manzanillo, Colima
# Requisitos: coloca los archivos procesados en data/processed/ (ve el ETL entregado)
# - data/processed/ageb_factibilidad.gpkg
# - data/processed/denue_salud.gpkg
# - data/processed/egresos_all.csv
# - data/processed/delitos_manzanillo.csv

library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(dplyr)
library(readr)
library(plotly)
library(DT)

# Opcional: osrm para isócronas. Si no lo tienes, la app hará buffers de distancia como fallback.
# install.packages('osrm') (requiere un servidor OSRM accesible o usar la API pública con límites)
has_osrm <- requireNamespace("osrm", quietly = TRUE)

# -------------------------
# Helper: carga segura de datos (si no existen, crea dummies)
# -------------------------
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

a <- st_read(ageb_fp, 
#        layer = layer, 
        quiet = TRUE) #%>% st_transform(4326)

a %>% st_transform(4326)
# Rutas de datos
ageb_fp <- "data/processed/ageb_factibilidad.gpkg"
denue_fp <- "data/processed/denue_salud.gpkg"
egresos_fp <- "data/processed/egresos_all.csv"
delitos_fp <- "data/processed/delitos_manzanillo.csv"

ageb <- st_read(ageb_fp, quiet = TRUE) %>% st_transform(4326)
denue <- st_read(denue_fp, quiet = TRUE) %>% st_transform(4326)

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
ageb %>% glimpse()

# si denue está vacío, crear dummies
if (is.null(denue)) {
  message("Generando puntos DENUE dummy...")
  pts <- st_sample(ageb, size = 300)
  denue <- st_as_sf(data.frame(pt = 1:length(pts)), geometry = unlist(pts))
  denue$Nombre <- paste0("Estab ", seq_len(nrow(denue)))
  denue$Clase_actividad <- sample(c("Farmacia","Clínica","Consultorio","Laboratorio"), nrow(denue), replace = TRUE)
}

# -------------------------
# Funciones para isócronas / fallback
# -------------------------
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

# -------------------------
# UI
# -------------------------
ui <- dashboardPage(
  dashboardHeader(title = "Factibilidad Hospitalaria — Manzanillo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      menuItem("KPIs", tabName = "kpis", icon = icon("tachometer-alt")),
      menuItem("Análisis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Datos", tabName = "data", icon = icon("table"))
    ),
    hr(),
    sliderInput("scoreFilter", "Filtro: score demanda (min-max)", min = min(ageb$score_demanda, na.rm = TRUE),
                max = max(ageb$score_demanda, na.rm = TRUE), value = c(min(ageb$score_demanda, na.rm = TRUE),
                                                                       max(ageb$score_demanda, na.rm = TRUE))),
    checkboxInput("show_denue", "Mostrar establecimientos (DENUE)", value = TRUE),
    checkboxInput("show_egresos", "Mostrar egresos (puntos)", value = TRUE),
    selectInput("giro_filter", "Giro DENUE", choices = c("Todos", sort(unique(denue$Clase_actividad))), selected = "Todos"),
    hr(),
    h4("Análisis de accesibilidad"),
    p("Haz clic en el mapa para seleccionar un punto (o usa el botón 'Calcular isócronas')."),
    actionButton("btn_calc_iso", "Calcular isócronas 15/30 min"),
    checkboxInput("show_iso", "Mostrar isócronas", value = TRUE),
    p("Nota: si no tienes un servidor OSRM accesible, la app usará un fallback basado en buffers (distancia estimada por velocidad media).")
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              fluidRow(
                box(width = 8, leafletOutput("map", height = 800)),
                box(width = 4, title = "Detalles selección", status = "info",
                    htmlOutput("info_selected"),
                    hr(),
                    plotlyOutput("time_series", height = 250))
              )
      ),
      tabItem(tabName = "kpis",
              fluidRow(
                valueBoxOutput("vb_pobl", width = 3),
                valueBoxOutput("vb_egresos", width = 3),
                valueBoxOutput("vb_estab", width = 3),
                valueBoxOutput("vb_delitos", width = 3)
              ),
              fluidRow(box(width = 12, title = "Mapa de score de factibilidad", leafletOutput("mini_map", height = 400)))
      ),
      tabItem(tabName = "analysis",
              fluidRow(box(width = 6, title = "Egresos por mes", plotlyOutput("egresos_plot")),
                       box(width = 6, title = "Delitos por tipo", plotlyOutput("delitos_plot")))
      ),
      tabItem(tabName = "data",
              fluidRow(box(width = 12, DT::dataTableOutput("table_ageb")))
      )
    )
  )
)

# -------------------------
# Server
# -------------------------
server <- function(input, output, session) {
  
  selected_pt <- reactiveVal(NULL)
  iso_sf <- reactiveVal(NULL)
  
  # Filtrar AGEB por score
  ageb_filtered <- reactive({
    ageb %>% filter(score_demanda >= input$scoreFilter[1], score_demanda <= input$scoreFilter[2])
  })
  
  denue_filtered <- reactive({
    df <- denue
    if (!is.null(input$giro_filter) && input$giro_filter != "Todos") {
      df <- df %>% filter(Clase_actividad == input$giro_filter)
    }
    df
  })
  
  output$map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", domain = ageb$score_demanda)
    m <- leaflet() %>% addProviderTiles("CartoDB.Positron")
    m <- m %>% addPolygons(data = ageb, fillColor = ~pal(score_demanda), fillOpacity = 0.7,
                           color = "#444", weight = 0.3, label = ~label, group = "ageb")
    # AGEB filtered outline
    m <- m %>% addPolygons(data = ageb_filtered(), fill = FALSE, color = "blue", weight = 1.4, group = "filtered")
    
    # DENUE
    if (input$show_denue) {
      m <- m %>% addCircleMarkers(data = denue_filtered(), radius = 5, stroke = FALSE,
                                  fillOpacity = 0.8, group = "denue",
                                  popup = ~paste0("<b>", Nombre, "</b><br>", Clase_actividad))
    }
    
    # Egresos (si hay columnas con coords, intentar plot)
    if (input$show_egresos && !is.null(egresos)) {
      if (all(c("latitud","longitud") %in% names(egresos))) {
        eg_sf <- st_as_sf(egresos, coords = c("longitud","latitud"), crs = 4326, remove = FALSE)
        # normalizar tamaño si existe TOTAL_EGRESOS
        if ("TOTAL_EGRESOS" %in% names(egresos)) {
          radii <- scales::rescale(egresos$TOTAL_EGRESOS, to = c(3,10))
        } else radii <- 5
        m <- m %>% addCircleMarkers(data = eg_sf, radius = radii,
                                    color = "#1f78b4", group = "egresos", popup = ~paste0(CLUES, "<br>", TOTAL_EGRESOS))
      }
    }
    
    m %>% addLegend(position = "bottomright", pal = pal, values = ageb$score_demanda, title = "Score demanda")
  })
  
  # click en el mapa
  observeEvent(input$map_click, {
    click <- input$map_click
    if (is.null(click)) return()
    lon <- click$lng; lat <- click$lat
    selected_pt(c(lon=lon, lat=lat))
    output$info_selected <- renderUI({
      HTML(paste0("Punto seleccionado: <b>", round(lat,6), ", ", round(lon,6), "</b><br>",
                  "Presiona 'Calcular isócronas' para estimar zonas de 15/30 min."))
    })
  })
  
  # Calcular isócronas cuando se presiona el botón
  observeEvent(input$btn_calc_iso, {
    pt <- selected_pt()
    if (is.null(pt)) {
      showNotification("Primero, haz clic en el mapa para seleccionar el punto de interés.", type = "error")
      return()
    }
    lon <- pt['lon']; lat <- pt['lat']
    # Primero intentar osrm
    iso_res <- NULL
    if (has_osrm) {
      iso_res <- compute_isochrones_osrm(lon, lat, breaks = c(15,30))
      if (!is.null(iso_res)) message("Isócronas calculadas con OSRM")
    }
    # Fallback
    if (is.null(iso_res)) {
      iso_res <- compute_isochrones_buffer(lon, lat, breaks = c(15,30), speed_kmh = 40)
      message("Isócronas calculadas con fallback por distancia (estimada a 40 km/h)")
    }
    iso_sf(iso_res)
    
    # Dibujar en mapa
    leafletProxy("map") %>% clearGroup("isochrones")
    if (input$show_iso && !is.null(iso_res)) {
      # osrm returns field 'min' or 't' depending; unify
      if ("min" %in% names(iso_res)) {
        mins <- iso_res$min
      } else if ("t" %in% names(iso_res)) {
        mins <- iso_res$t
      } else if ("breaks" %in% names(iso_res)) {
        mins <- iso_res$breaks
      } else mins <- 1:nrow(iso_res)
      
      pal_iso <- colorFactor(c("#1a9850","#fdae61"), domain = mins)
      for (i in seq_len(nrow(iso_res))) {
        m <- leafletProxy("map") %>%
          addPolygons(data = iso_res[i,], fillColor = pal_iso(mins[i]), fillOpacity = 0.3,
                      color = NA, group = "isochrones",
                      label = paste0(mins[i], " min"))
      }
    }
  })
  
  # KPIs
  output$vb_pobl <- renderValueBox({
    tot <- sum(ageb$poblacion, na.rm = TRUE)
    valueBox(formatC(tot, format = "d", big.mark = ","), "Población total (AGEB)", icon = icon("users"))
  })
  
  output$vb_egresos <- renderValueBox({
    v <- if(!is.null(egresos) && "TOTAL_EGRESOS" %in% names(egresos)) sum(egresos$TOTAL_EGRESOS, na.rm = TRUE) else NA
    valueBox(ifelse(is.na(v), "N/D", formatC(v, format = "d", big.mark = ",")), "Egresos (total)", icon = icon("hospital"))
  })
  
  output$vb_estab <- renderValueBox({
    n <- nrow(denue)
    valueBox(n, "Establecimientos (DENUE)", icon = icon("store"))
  })
  
  output$vb_delitos <- renderValueBox({
    v <- if(!is.null(delitos)) sum(delitos$Incidencia, na.rm = TRUE) else NA
    valueBox(ifelse(is.na(v), "N/D", formatC(v, format = "d", big.mark = ",")), "Incidencia delictiva (total)", icon = icon("exclamation-triangle"))
  })
  
  output$mini_map <- renderLeaflet({
    pal <- colorNumeric("YlOrRd", domain = ageb$score_demanda)
    leaflet() %>% addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = ageb, fillColor = ~pal(score_demanda), fillOpacity = 0.9, color = "#444", weight = 0.4) %>%
      addLegend(position = "bottomright", pal = pal, values = ageb$score_demanda, title = "Score")
  })
  
  # Series: egresos por año
  output$egresos_plot <- renderPlotly({
    if (is.null(egresos)) return(NULL)
    df <- egresos %>% group_by(ano) %>% summarise(total = sum(TOTAL_EGRESOS, na.rm = TRUE))
    plot_ly(df, x = ~ano, y = ~total, type = 'scatter', mode = 'lines+markers') %>%
      layout(title = 'Egresos por año', xaxis = list(title = 'Año'), yaxis = list(title = 'Egresos'))
  })
  
  output$delitos_plot <- renderPlotly({
    if (is.null(delitos)) return(NULL)
    df <- delitos %>% group_by(Tipo.de.Delito) %>% summarise(total = sum(Incidencia, na.rm = TRUE)) %>% arrange(-total)
    plot_ly(df, x = ~reorder(Tipo.de.Delito, total), y = ~total, type = 'bar') %>%
      layout(title = 'Incidencia por delito', xaxis = list(title = ''), yaxis = list(title = 'Casos'))
  })
  
  output$time_series <- renderPlotly({
    if (is.null(egresos)) return(NULL)
    df <- egresos %>% group_by(ano) %>% summarise(total = sum(TOTAL_EGRESOS, na.rm = TRUE))
    plot_ly(df, x=~ano, y=~total, type='scatter', mode='lines+markers')
  })
  
  output$table_ageb <- DT::renderDataTable({
    df <- ageb %>% st_set_geometry(NULL)
    DT::datatable(df)
  })
  
  # info inicial
  output$info_selected <- renderUI({
    HTML("Haz clic en el mapa para seleccionar un punto. Luego presiona 'Calcular isócronas' para 15/30 min.")
  })
  
}

shinyApp(ui, server)
