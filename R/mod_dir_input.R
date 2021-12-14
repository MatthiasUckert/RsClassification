#' dir_input UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_dir_input_ui <- function(id){
  ns <- NS(id)
  tagList(
    textInput(
      inputId = ns("dir"), 
      label = "Full Path to the Directory", 
      placeholder = "Directory Path"
    ),
    hr(),
    actionButton(ns("start_project"), label = "Start Project"),
    h3("Overview"),
    tableOutput(ns("overview"))
  )
}
    
#' dir_input Server Functions
#'
#' @noRd 
mod_dir_input_server <- function(id, input) {
  doc_id <- file_ext <- Variable <- Value <- file_ext <- n <- NULL
  
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    r <- reactive({
      input$start_project
      input$dir
      
      files_ <- lft(input$dir, "input.*xlsx")
      
      if (nrow(files_) == 0) {
        stop("No Input File Found", call. = FALSE)
      } else if (nrow(files_) > 1) {
        stop("More than one Input File Found", call. = FALSE)
      }
      
      path_excel_  <- files_[["path"]]
      check_excel_ <- file.exists(path_excel_)
      path_docs_   <- file.path(input$dir, "docs")
      check_docs_  <- file.exists(path_docs_)
      path_data_   <- file.path(input$dir, "data", "data.rds")
      check_data_  <- file.exists(path_data_)
      
      name_project_ <- janitor::make_clean_names(basename(input$dir))
      
      
      if (check_excel_) {
        tab_spec_ <- tibble::as_tibble(openxlsx::read.xlsx(path_excel_, "spec"))
        tab_data_ <- tibble::as_tibble(openxlsx::read.xlsx(path_excel_, "data", detectDates = TRUE)) %>%
          dplyr::arrange(id)
        
        if (!all.equal(sort(tab_spec_$col), sort(tab_spec_$col))) {
          stop("Column Specifications don't match columns", call. = FALSE)
        } else {
          
          for (i in seq_len(nrow(tab_spec_))) {
            col_ <- tab_spec_$col[i]
            type_ <- tab_spec_$type[i]
            if (type_ %in% c("character", "dropdown")) {
              tab_data_[[col_]] <- as.character(tab_data_[[col_]])
            } else if (type_ == "numeric") {
              tab_data_[[col_]] <- as.numeric(tab_data_[[col_]])
            } else if (type_ == "date") {
              tab_data_[[col_]] <- as.Date(tab_data_[[col_]], format = "%Y-%m-%d")
            }
          }
        }

        tab_data_ <- tab_data_[, tab_spec_$col]
        tab_fils_ <- lft(path_docs_)
        tab_docs_ <- openxlsx::read.xlsx(path_excel_, "docs") %>%
          tibble::as_tibble() %>%
          dplyr::left_join(
            y = tidyr::unite(tab_fils_, doc_id, doc_id, file_ext, sep = ""), 
            by = "doc_id"
            )

      } else { 
        tab_data_ <- tab_coltypes_ <- tab_docs_ <- tab_fils_ <- tab_cols_ <- tibble::tibble()
      }
      
      list(
        dir_project = input$dir,
        path_excel = path_excel_,
        check_excel = check_excel_,
        path_docs = path_docs_,
        check_docs = check_docs_, 
        path_data = path_data_, 
        check_data = check_data_,
        name_project = name_project_,
        data = tab_data_,
        cols = tab_spec_,
        docs = tab_docs_,
        fils = tab_fils_
      )
    })
    
    
    ftable <- function() {
      req(input$dir)
      if (r()$check_excel) {
        tab_ <- tibble::tribble(
          ~Variable, ~ Value,
          "Directory", ft_italic(r()$dir_project),
          "Input File Exists", ft_TRUE_FALSE(r()$check_excel),
          "Document Folder Exists", ft_TRUE_FALSE(r()$check_docs),
          "Project Intitialized", ft_TRUE_FALSE(r()$check_data)
        ) %>%
          tibble::add_row(Variable = "", Value = "") %>%
          tibble::add_row(Variable = ft_bold("Entries"), Value = ft_bold("N")) %>% 
          dplyr::bind_rows(
            r()$data %>%
              dplyr::mutate(Variable = "IDs") %>%
              dplyr::group_by(Variable) %>%
              dplyr::summarise(Value = dplyr::n()) %>%
              dplyr::mutate(Value = scales::comma(Value, 1))
          ) %>%
          tibble::add_row(Variable = "", Value = "") %>%
          tibble::add_row(Variable = ft_bold("Files"), Value = ft_bold("N")) %>% 
          dplyr::bind_rows(
            r()$fils %>%
              dplyr::count(file_ext) %>%
              dplyr::rename(Variable = file_ext, Value = n) %>%
              janitor::adorn_totals() %>%
              dplyr::mutate(Value = scales::comma(Value, 1))
          )
      } else {
        tab_ <- tibble::tibble(Message = "No Project Found")
      }
 
      tab_ %>%
        kableExtra::kbl(escape = FALSE) %>%
        kableExtra::kable_paper() %>%
        kableExtra::kable_styling(position = "left", full_width = TRUE) %>%
        kableExtra::row_spec(row = nrow(tab_), bold = TRUE)
    }
    
    output$overview <- function() ftable()
    
    
    rstart <- reactiveVal(FALSE)
    observeEvent(input$dir, {rstart(FALSE)})
    observeEvent(input$start_project, {
      if (!r()$check_data) {
        dir.create(dirname(r()$path_data), FALSE, TRUE)
        readr::write_rds(r()$data, r()$path_data, "gz")
      }
      output$overview <- function() ftable()
      rstart(TRUE)
      })
    
    rout <- reactive({
      input$start_project
      
      if (rstart()) {
        list(start = rstart(), values = r())
      } else {
        list(start = rstart())
      } 
    })
    
    return(rout)
    
    
  })
}


## To be copied in the UI
# mod_dir_input_ui("dir_input_ui_1")
    
## To be copied in the server
# mod_dir_input_server("dir_input_ui_1")



# Debug -------------------------------------------------------------------
# library(tidyverse)
# options(shiny.autoreload = TRUE)
# source("../R/golem_utils_ui.R")
# source("../R/utils_helpers.R")
# 
# 
# ui <- fluidPage(mod_dir_input_ui("dir_input_ui_1"))
# server <- function(input, output, session) {
#   r <- mod_dir_input_server("dir_input_ui_1", input)
# 
#   observe(print(r()))
# 
# }
# shinyApp(ui, server)
