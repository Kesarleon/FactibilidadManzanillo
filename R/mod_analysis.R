# R/mod_analysis.R

mod_analysis_ui <- function(id) {
  ns <- NS(id)
  fluidRow(box(width = 6, title = "Egresos por mes", plotlyOutput(ns("egresos_plot"))),
           box(width = 6, title = "Delitos por tipo", plotlyOutput(ns("delitos_plot"))))
}

mod_analysis_server <- function(id, egresos, delitos) {
  moduleServer(id, function(input, output, session) {

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

  })
}
