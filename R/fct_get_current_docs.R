#' get_current_docs 
#'
#' @description A fct function
#'
#' @return The return value, if any, from executing the function.
#'
#' @noRd
fct_get_current_docs <- function(.tab, .id) {
  id <- doc_id <- ext <- name <- path <- NULL
  .tab %>%
    dplyr::filter(id == .id) %>%
    dplyr::mutate(
      ext = toupper(tools::file_ext(doc_id)),
      ext = dplyr::case_when(
        ext %in% "PDF" ~ "PDF",
        ext %in% c("HTM", "HTML") ~ "HTML",
        TRUE ~ "Image"
      )
    ) %>%
    dplyr::group_by(ext) %>%
    dplyr::mutate(name = paste(toupper(ext), dplyr::row_number())) %>%
    dplyr::ungroup() %>%
    dplyr::select(id, name, ext, path)
}