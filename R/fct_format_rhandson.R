#' format_rhandson 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_format_rhandson <- function(.tab, .specs) {
  tab_ <- .tab %>%
    rhandsontable::rhandsontable(useTypes = TRUE, rowHeaders = FALSE, overflow = "visible")  %>%
    rhandsontable::hot_cols(manualColumnResize = TRUE, halign = "htCenter", valign = 'htCenter', colWidths = 100) %>%
    rhandsontable::hot_rows(rowHeights = 100) %>%
    rhandsontable::hot_col(col = "id", readOnly = TRUE)
  
  for (i in seq_len(nrow(.specs))) {
    col_ <- .specs$col[i]
    type_ <- .specs$type[i]
    val_ <- unlist(stringi::stri_split_fixed(.specs$vals[i], "|"))
    
    if (!type_ == "character") {
      if (all(!is.na(val_))) {
        tab_ <- rhandsontable::hot_col(tab_, col = col_, type = type_, source = val_)
      } else {
        tab_ <- rhandsontable::hot_col(tab_, col = col_, type = type_)
      }
    }
  }
  
  return(tab_)
}


# DF <- data.frame(
#   val1 = 1:10,
#   val2 = rep(NA_character_, 10),
#   val3 = rep(NA_character_, 10)
# )
# 
# .specs <- tibble::tribble(
#   ~col, ~type, ~vals,
#   "val1", "character", NA_character_,
#   "val2", "dropdown", "A|B|C",
#   "val3", "dropdown", "D|E|F"
# )
# 
# tab_ <- DF %>%
#   rhandsontable::rhandsontable(useTypes = TRUE, rowHeaders = FALSE) %>%
#   rhandsontable::hot_cols(manualColumnResize = TRUE, halign = "htCenter", valign = "htCenter", colWidths = 100) %>%
#   rhandsontable::hot_rows(rowHeights = 50)
# 
# for (i in seq_len(nrow(.specs))) {
#   col_ <- .specs$col[i]
#   type_ <- .specs$type[i]
#   val_ <- unlist(stringi::stri_split_fixed(.specs$vals[i], "|"))
# 
#   if (!type_ == "character") {
#     if (all(!is.na(val_))) {
#       tab_ <- rhandsontable::hot_col(tab_, col = col_, type = type_, source = val_)
#     } else {
#       tab_ <- rhandsontable::hot_col(tab_, col = col_, type = type_)
#     }
#   }
# }
# 
# tab_
# 
# 
# rhandsontable(DF, rowHeaders = NULL) %>%
#   hot_col(col = "big", type = "dropdown", source = LETTERS) %>%
#   hot_col(col = "small", type = "autocomplete", source = letters,
#           strict = FALSE)