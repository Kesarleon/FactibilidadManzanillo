# R/mod_map.R

# UI de la Sidebar para el módulo Mapa
mod_map_ui_sidebar <- function(id) {
  ns <- NS(id)
  tagList(
    sliderInput(ns("scoreFilter"), "Filtro: score demanda (min-max)",
                min = 0, max = 1, value = c(0, 1)), # Rango inicial dummy, se actualiza en servidor o global
    checkboxInput(ns("show_denue"), "Mostrar establecimientos (DENUE)", value = TRUE),
    checkboxInput(ns("show_egresos"), "Mostrar egresos (puntos)", value = TRUE),
    selectInput(ns("giro_filter"), "Giro DENUE", choices = c("Todos"), selected = "Todos"),
    hr(),
    h4("Análisis de accesibilidad"),
    p("Haz clic en el mapa para seleccionar un punto (o usa el botón 'Calcular isócronas')."),
    actionButton(ns("btn_calc_iso"), "Calcular isócronas 15/30 min"),
    checkboxInput(ns("show_iso"), "Mostrar isócronas", value = TRUE),
    p("Nota: si no tienes un servidor OSRM accesible, la app usará un fallback basado en buffers (distancia estimada por velocidad media).")
  )
}

# UI del Cuerpo para el módulo Mapa
mod_map_ui_body <- function(id) {
  ns <- NS(id)
  fluidRow(
    box(width = 8, leafletOutput(ns("map"), height = 800)),
    box(width = 4, title = "Detalles selección", status = "info",
        htmlOutput(ns("info_selected")),
        hr(),
        plotlyOutput(ns("time_series"), height = 250))
  )
}

# Servidor del módulo Mapa
mod_map_server <- function(id, ageb, denue, egresos, has_osrm) {
  moduleServer(id, function(input, output, session) {

    # Actualizar inputs de rango/opciones basados en datos reales
    observe({
      if (!is.null(ageb)) {
        min_s <- min(ageb$score_demanda, na.rm = TRUE)
        max_s <- max(ageb$score_demanda, na.rm = TRUE)
        updateSliderInput(session, "scoreFilter", min = min_s, max = max_s, value = c(min_s, max_s))
      }
      if (!is.null(denue)) {
        choices <- c("Todos", sort(unique(denue$Clase_actividad)))
        updateSelectInput(session, "giro_filter", choices = choices, selected = "Todos")
      }
    })

    selected_pt <- reactiveVal(NULL)
    iso_sf <- reactiveVal(NULL)

    # Filtrar AGEB por score
    ageb_filtered <- reactive({
      req(input$scoreFilter)
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

      # Agregar marcador visual
      leafletProxy(ns("map")) %>%
        clearGroup("selected_pt") %>%
        addMarkers(lng = lon, lat = lat, group = "selected_pt")

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

    output$time_series <- renderPlotly({
      if (is.null(egresos)) return(NULL)
      df <- egresos %>% group_by(ano) %>% summarise(total = sum(TOTAL_EGRESOS, na.rm = TRUE))
      plot_ly(df, x=~ano, y=~total, type='scatter', mode='lines+markers')
    })

    # info inicial
    output$info_selected <- renderUI({
      HTML("Haz clic en el mapa para seleccionar un punto. Luego presiona 'Calcular isócronas' para 15/30 min.")
    })

  })
}
