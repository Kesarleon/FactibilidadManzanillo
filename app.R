# app.R
# Shiny app: Mapa interactivo de factibilidad de hospital - Manzanillo, Colima
# Estructura modular

# Los datos globales y librerías se cargan en global.R (o automáticamente si está en el mismo dir)
# source("global.R") # A veces necesario en local, pero shinyapps lo hace auto.
# Para asegurar, si global.R no se ha ejecutado (p. ej. testing manual):
if(!exists("ageb")) source("global.R")

ui <- dashboardPage(
  dashboardHeader(title = "Factibilidad Hospitalaria — Manzanillo"),
  dashboardSidebar(
    sidebarMenu(id = "tabs",
      menuItem("Mapa", tabName = "map", icon = icon("map")),
      menuItem("KPIs", tabName = "kpis", icon = icon("tachometer-alt")),
      menuItem("Análisis", tabName = "analysis", icon = icon("chart-line")),
      menuItem("Datos", tabName = "data", icon = icon("table"))
    ),
    hr(),
    # Inputs específicos del módulo Mapa, visibles solo cuando tab == map
    # Usamos conditionalPanel. Nota: input.tabs se refiere al id del sidebarMenu
    conditionalPanel(
      condition = "input.tabs == 'map'",
      mod_map_ui_sidebar("map1")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "map",
              mod_map_ui_body("map1")
      ),
      tabItem(tabName = "kpis",
              mod_kpis_ui("kpis1")
      ),
      tabItem(tabName = "analysis",
              mod_analysis_ui("analysis1")
      ),
      tabItem(tabName = "data",
              mod_data_ui("data1")
      )
    )
  )
)

server <- function(input, output, session) {

  # Llamada a los servidores de módulos
  # Pasamos los datos globales 'ageb', 'denue', etc.
  mod_map_server("map1", ageb, denue, egresos, has_osrm)
  mod_kpis_server("kpis1", ageb, denue, egresos, delitos)
  mod_analysis_server("analysis1", egresos, delitos)
  mod_data_server("data1", ageb)

}

shinyApp(ui, server)
