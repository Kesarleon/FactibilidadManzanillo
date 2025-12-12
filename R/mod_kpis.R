# R/mod_kpis.R

mod_kpis_ui <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      valueBoxOutput(ns("vb_pobl"), width = 3),
      valueBoxOutput(ns("vb_egresos"), width = 3),
      valueBoxOutput(ns("vb_estab"), width = 3),
      valueBoxOutput(ns("vb_delitos"), width = 3)
    ),
    fluidRow(box(width = 12, title = "Mapa de score de factibilidad", leafletOutput(ns("mini_map"), height = 400)))
  )
}

mod_kpis_server <- function(id, ageb, denue, egresos, delitos) {
  moduleServer(id, function(input, output, session) {

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

  })
}
