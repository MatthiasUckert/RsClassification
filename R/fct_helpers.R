#' helpers 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_helper_convert_ext <- function(.path) {
  ext_ = toupper(tools::file_ext(.path))
  dplyr::case_when(
    ext_ %in% "PDF" ~ "PDF",
    ext_ %in% c("HTM", "HTML") ~ "HTML",
    TRUE ~ "Image"
  )
}
