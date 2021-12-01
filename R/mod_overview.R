#' overview UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
mod_overview_ui <- function(id){
  ns <- NS(id)
  tagList(
    plotOutput(ns("overview"))
  )
}
    
#' overview Server Functions
#'
#' @noRd 
mod_overview_server <- function(id, .init) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    output$overview <- renderPlot({
      if (!is.null(.init()$values$dir_project)) {
        tmp1 <- openxlsx::read.xlsx(file.path(.init()$values$dir_project, "input.xlsx")) %>%
          tibble::as_tibble() %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
          tidyr::pivot_longer(!dplyr::matches("^id$")) %>%
          dplyr::group_by(name) %>%
          dplyr::summarise(value = sum(!is.na(value)) / dplyr::n()) %>%
          dplyr::mutate(type = "Original")

        tmp2 <- readr::read_rds(file.path(.init()$values$dir_project, "data/data.rds")) %>%
          tibble::as_tibble() %>%
          dplyr::mutate(dplyr::across(dplyr::everything(), as.character)) %>%
          tidyr::pivot_longer(!dplyr::matches("^id$")) %>%
          dplyr::group_by(name) %>%
          dplyr::summarise(value = sum(!is.na(value)) / dplyr::n()) %>%
          dplyr::mutate(type = "Processed")

        dplyr::bind_rows(tmp1, tmp2) %>%
          ggplot2::ggplot(ggplot2::aes(name, value, fill = type)) +
          ggplot2::geom_col(position = "dodge") +
          ggplot2::scale_fill_manual(values = c("lightblue", "lightgreen")) +
          ggplot2::scale_y_continuous(labels = scales::percent, name = "Attributes Completed") +
          ggplot2::xlab(NULL) +
          ggplot2::geom_hline(yintercept = .25, linetype = "dashed", color = "lightgrey") +
          ggplot2::geom_hline(yintercept = .50, linetype = "dashed", color = "lightgrey") +
          ggplot2::geom_hline(yintercept = .75, linetype = "dashed", color = "lightgrey") +
          ggplot2::geom_hline(yintercept = 1.0, linetype = "dashed", color = "lightgrey") +
          ggthemes::geom_rangeframe() +
          ggthemes::theme_tufte(base_size = 18) +
          ggplot2::theme(
            axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1, size = 18),
            legend.position = "top",
            legend.title = ggplot2::element_blank()
          )
      }
    }, height = 500)
  })
}
    
## To be copied in the UI
# mod_overview_ui("overview_ui_1")
    
## To be copied in the server
# mod_overview_server("overview_ui_1")



