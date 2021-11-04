#' format_dt 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_format_dt <- function(.tab, .pagelen = 5, .page = 0, .sel = 0) {
  DT::datatable(
    data      = .tab,
    rownames  = FALSE,
    filter    = "none",
    style     = "default", 
    class     = "table-bordered table-condensed hover compact",
    options = list(
      pageLength   = .pagelen, 
      searching    = TRUE, 
      lengthChange = TRUE,
      dom = "ltip",
      pagingType = "simple",
      displayStart = .page
    ),
    selection = list(mode = 'single', target = 'row', selected = .sel)
  )
}
