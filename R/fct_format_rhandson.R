#' format_rhandson 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_format_rhandson <- function(.tab, .col_types) {
  tab_ <- .tab %>%
    rhandsontable::rhandsontable(useTypes = TRUE, rowHeaders = FALSE)  %>%
    rhandsontable::hot_cols(manualColumnResize = TRUE, halign = "htCenter") %>%
    rhandsontable::hot_rows(rowHeights = 20) %>%
    rhandsontable::hot_col(col = "id", readOnly = TRUE)
  
  if (nrow(.col_types) > 0) {
    for (i in seq_len(nrow(.col_types))) {
      col_ <- .col_types$name[i]
      type_ <- .col_types$value[i]
      tab_ <- rhandsontable::hot_col(tab_, col = col_, type = type_)
    }
  }
  
  return(tab_)
}