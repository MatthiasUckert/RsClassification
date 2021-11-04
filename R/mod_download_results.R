#' download_results UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_download_results_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      style="display: inline-block; width: 400px;",
      downloadButton(ns("download"), label = "Download Results"),
      " (as .csv file '|' separated)"
    ),
    
  )
}
    
#' download_results Server Functions
#'
#' @noRd 
mod_download_results_server <- function(id, .name, .data) {
  moduleServer( id, function(input, output, session){
    ns <- session$ns
    
    output$download <- downloadHandler(
      filename = function() {
        paste0(format(Sys.time(), "%Y-%m-%d-%H-%M-%S"), "_", .name, ".csv")
      },
      content = function(.file) {
        readr::write_delim(.data, .file, delim = "|", na = "")
        },
      contentType = "text/csv"
    )
  })
}
    
## To be copied in the UI
# mod_download_results_ui("download_results_ui_1")
    
## To be copied in the server
# mod_download_results_server("download_results_ui_1")

# demo <- function() {
#   ui <- fluidPage(
#     mod_download_results_ui("download_results_ui_1")
#   )
# 
#   server <- function(input, output, session) {
#     mod_download_results_server("download_results_ui_1", "test.csv", mtcars)
#   }
#   shinyApp(ui, server)
# }
# 
# demo()
