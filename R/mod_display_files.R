#' display_files UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_display_files_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(
      style="display: inline-block; width: 400px;",
      sliderInput(ns("height"), "Frame Height", min = 50, max = 2500, value = 800, step = 50)
    ),
    div(
      style="display: inline-block; width: 400px;",
      sliderInput(ns("width"), "Frame Width", min = 50, max = 2500, value = 1200, step = 50)
    ),
    tabsetPanel(
      id = ns("docs"),
      type = "hidden",
      tabPanel(
        title = "None"
      ),
      tabPanel(
        title = "HTML",
        h4("HTML Document"),
        htmlOutput(ns("html_file"))
      ),
      tabPanel(
        title = "PDF",
        h4("PDF Document"),
        uiOutput(ns("pdf_file"))
      ),
      tabPanel(
        title = "Image",
        h4("Image Document"),
        htmlOutput(ns("img_data")),
        plotOutput(ns("img_file"))
      )
    ) 
  )
}
    
#' display_files Server Functions
#'
#' @noRd 
mod_display_files_server <- function(id, .reactive_path) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    observe({
      updateTabsetPanel(inputId = "docs", selected = fct_helper_convert_ext(.reactive_path()))
    })


    output$img_file <- renderImage({
        list(
          seamless = "seamless",
          src = .reactive_path(),
          width = input$width,
          height = input$height
        )
      },deleteFile = FALSE)

    output$html_file <- renderUI({
      try(addResourcePath("docs", dirname(.reactive_path())), silent = TRUE)
      tags$iframe(
        seamless = "seamless",
        src = file.path("docs", basename(.reactive_path())),
        width = input$width,
        height = input$height
      )
    })

    output$pdf_file <- renderUI({
     try(addResourcePath("docs", dirname(.reactive_path())), silent = TRUE)
      tags$iframe(
        seamless = "seamless",
        src = file.path("docs", basename(.reactive_path())),
        width = input$width,
        height = input$height
      )
    })

    output$img_data <- renderText({
      d_ <- try(dim(magick::image_data(magick::image_read(.reactive_path()))), silent = TRUE)

      if (inherits(d_, "try-error")) {
        paste0(
          strong("Could not retrieve Image Dimensions")
        )
      } else {
        paste0(
          strong("Original Image Height: "), scales::comma(d_[3], 1), " Pixel",
          "; ",
          strong("Original Image Width: "), scales::comma(d_[2], 1), " Pixel"
        )
      }
    })
  })
}
    
## To be copied in the UI
# mod_display_files_ui("display_files_ui_1")
    
## To be copied in the server
# mod_display_files_server("display_files_ui_1")

# demo <- function() {
#   ui <- fluidPage(
#     fileInput("file", "File"),
#     mod_display_files_ui("display_files_ui_1")
#     )
# 
#   server <- function(input, output, session) {
#     r <- reactive(input$file$datapath)
#     mod_display_files_server("display_files_ui_1", r)
#     
#     print_reactive(r)
#   }
#   shinyApp(ui, server)
# }
# 
# demo()
