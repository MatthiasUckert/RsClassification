#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  path <- ext <- . <- id <- prc <- per <- NULL
  
  # Reactive Values -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  rinit <- mod_dir_input_server("dir_input_ui_1")
  rVdata <- reactiveVal()
  rVpage <- reactiveVal(0)
  rVpage_length <- reactiveVal(5)
  
  observe({
    if (rinit()$start) {
      rVdata(readr::read_rds(rinit()$values$path_data))
    }
  })
  
  observeEvent(input$submit, {
    rVpage(input$excel_table_data_dt_rows_current[1] - 1)
    rVpage_length(length(input$excel_table_data_dt_rows_current))
  })
  
  rlrc_data <- fct_lrc(input, "excel_table_data_dt")
  rlrc_docs <- fct_lrc(input, "excel_table_docs_dt")
  
  rdocs_row <- reactive({
    if (rinit()$start) {
      fct_get_current_docs(rinit()$values$docs, .id = rVdata()[["id"]][rlrc_data()])
    }
  })
  
  rfile_path <- reactive(rdocs_row()[["path"]][rlrc_docs()])
  rid <- reactive(rVdata()[["id"]][rlrc_data()])
  
  # observe({print(rfile_path())})
  # observe({print(rdocs_row())})
  
  
  # Outputs -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  
  # Overview
  mod_overview_server("overview_ui_1", rinit)
  
  observe({print(rinit())})
  
  # Current ID
  output$html_id <- function() {
    paste0(h3(paste0("Current ID: ", rid())))
  } 
  
  output$doc_paths <- function() {
    paste(
      rdocs_row()[["path"]], collapse = "<br>"
    )
  }
  
  # Data: Datatable in SidePanel
  output$excel_table_data_dt <- DT::renderDataTable({
    rVdata() %>%
      dplyr::mutate(
        all = ncol(.) - 1,
        prc  = rowSums(!is.na(.[, -1])),
        per = prc / all
      ) %>%
      dplyr::select(id, `No. of Vars` = all, `Filled (%)` = per) %>%
      fct_format_dt(.page = rVpage(), .pagelen = rVpage_length()) %>%
      DT::formatPercentage(3) %>%
      DT::formatStyle(
        columns = 1:3,
        valueColumns = 3,
        backgroundColor = DT::styleInterval(
          cuts = c(.25, .50, .75),
          values = c("#EC7063", "#F5B041", "#F4D03F", "#58D68D")
        )
        
      )
  })
  
  # Docs: Datatable in MainPanel
  output$excel_table_docs_dt <- DT::renderDataTable({
    rdocs_row() %>%
      dplyr::select(-path, -ext) %>%
      fct_format_dt()
  })
  
  # Rhandsontable in MainPanel
  output$excel_table_data_rh <- rhandsontable::renderRHandsontable({
    rVdata() %>%
      dplyr::slice(rlrc_data()) %>%
      fct_format_rhandson(., rinit()$values$cols)
  })
  
  # Dislay Documents in MainPanel
  mod_display_files_server(
    id = "display_files_ui_1", 
    .reactive_path = rfile_path
  )
  
  # Download Results -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- --
  mod_download_results_server(
    id = "download_results_ui_1", 
    .name = rinit()$values$name_project, 
    .data = rVdata()
  )
  
  # Update Results -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -- -
  
  observeEvent(input$submit, {
    if (!is.null(input$excel_table_data_rh)) {
      tab_ <- rhandsontable::hot_to_r(input$excel_table_data_rh)
      tab_ <- dplyr::bind_rows(tab_, rVdata()) %>%
        dplyr::distinct(id, .keep_all = TRUE) %>%
        dplyr::as_tibble() %>%
        dplyr::arrange(id)
      readr::write_rds(tab_, rinit()$values$path_data, compress = "gz")
    }
    rVdata(tab_)
  }, priority = 1)
  
  
  print_reactive(rVpage)
  
  
}

# 
# tibble::tibble(
#   id = 1:5,
#   val1 = c(1, NA, 2, NA, 3),
#   val2 = c(NA, 1, 1, NA, 1)
# )  
