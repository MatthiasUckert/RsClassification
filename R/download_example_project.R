#' Download Example Project
#'
#' @param .dir Full Path to the Directory
#' @param .name Name of the Project
#'
#' @return A Folder containing Example Files
#' @export
download_example_project <- function(.dir, .name) {
  dir_ <- system.file("extdata/test_project", package = "RsClassification")
  check_ <- try(fs::dir_copy(dir_, .dir, overwrite = FALSE), silent = TRUE)
  
  if (!inherits(check_, "try-error")) {
    file.rename(file.path(.dir, "test_project"), file.path(.dir, .name))
    message("Project successfully created")
  } else {
    msg_ <- attr(check_, "condition")$message
    if (grepl("file already exists", msg_)) {
      message("Project already exists")
    } else {
      message(msg_)
    }
    
  }
  
  
  
}
