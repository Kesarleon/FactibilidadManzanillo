# R/mod_data.R

mod_data_ui <- function(id) {
  ns <- NS(id)
  fluidRow(box(width = 12, DT::dataTableOutput(ns("table_ageb"))))
}

mod_data_server <- function(id, ageb) {
  moduleServer(id, function(input, output, session) {

    output$table_ageb <- DT::renderDataTable({
      df <- ageb %>% st_set_geometry(NULL)
      DT::datatable(df)
    })

  })
}
