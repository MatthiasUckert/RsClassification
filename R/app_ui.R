#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic

    fluidPage(
      tabsetPanel(
        tabPanel(
          title = "File Input",
          mod_dir_input_ui("dir_input_ui_1")
        ),
        tabPanel(
          title = "Classification",
          sidebarLayout(
            sidebarPanel = sidebarPanel(
              width = 2,
              mod_download_results_ui("download_results_ui_1"),
              hr(),
              # textInput("user", "User Name", placeholder = "User Name"),
              DT::dataTableOutput("excel_table_data_dt")
            ),
            mainPanel = mainPanel(
              fluidRow(
                actionButton("submit", "Submit"),
                rhandsontable::rHandsontableOutput("excel_table_data_rh")
              ),
              hr(),
              fluidRow(
                htmlOutput("html_id"),
                col_02(
                  h4("Documents"), 
                  DT::dataTableOutput("excel_table_docs_dt") 
                ),
                col_10(
                  mod_display_files_ui("display_files_ui_1")
                )
              ) 
            )
          )
        )
      )
    )
  )
}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', app_sys('app/www')
  )
 
  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys('app/www'),
      app_title = 'RsClassification'
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert() 
  )
}

