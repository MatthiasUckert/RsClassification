#' Download Example Project
#'
#' @param .dir Full Path to the Directory
#' @param .name Name of the Project
#'
#' @return A Folder containing Example Files
#' @export
download_example_project <- function(.dir, .name) {
  dir_ <- system.file("extdata/test_project", package = "RsClassification")
  fs::dir_copy(dir_, .dir, overwrite = FALSE)
  file.rename(file.path(.dir, "test_project"), file.path(.dir, .name))
  
}