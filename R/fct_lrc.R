#' lrc 
#'
#' @description Returns the Last Row Clicked of a Datatable
#'
#' @return An Integer
#'
#' @noRd
fct_lrc <- function(input, .id) {
  reactive({
    lrc_ <- input[[glue::glue("{.id}_row_last_clicked")]]
    ifelse(is.null(lrc_), 1, lrc_)
  })
}