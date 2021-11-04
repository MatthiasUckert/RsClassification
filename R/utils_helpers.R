#' helpers 
#'
#' @description A utils function
#'
#' @return The return value, if any, from executing the utility.
#'
#' @noRd
print_reactive <- function(.reactive) {
  observe({
    print(.reactive())
  })
}

ft_TRUE_FALSE <- function(.lgl) {
  ifelse(
    .lgl,
    kableExtra::text_spec(.lgl, color = "green", bold = TRUE),
    kableExtra::text_spec(.lgl, color = "red", bold = TRUE)
  )
}

ft_italic <- function(.text) {
  kableExtra::text_spec(.text, italic = TRUE)
}

ft_bold <- function(.text) {
  kableExtra::text_spec(.text, bold = TRUE)
}

lft <- function(.dirs, .reg = "*", id = "doc_id", .rec = FALSE) {
  path <- file_ext <- doc_id <- NULL
  purrr::map_dfr(
    .x = .dirs, 
    .f = ~ tibble::tibble(
      path = list.files(.x, .reg, FALSE, TRUE, .rec)
      )) %>%
    dplyr::mutate(
      file_ext = paste0(".", tools::file_ext(path)), 
      doc_id =  stringi::stri_replace_last_fixed(basename(path), file_ext, ""), 
      path = purrr::set_names(path, !!dplyr::sym(id))) %>%
    dplyr::select(!!id, file_ext, path)
}
