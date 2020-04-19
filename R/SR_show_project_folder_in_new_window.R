#' Opens the project folder in Windows Explorer
#'
#' Opens the project folder in Windows Explorer.
#'
#' @return NULL, opens Windows Explorer
#'
#' @examples
#' SR_show_project_folder_in_new_window()
#'
#' @export
SR_show_project_folder_in_new_window <- function(){
  path <- getwd()
  si <- Sys.info()["sysname"]
  #
  if (si == "Darwin") {           # mac
    system2("open", path)
  } else if (si == "Windows") {   # win
    shell.exec(path)
  } else if (si == "Linux") {     # linux
    system(paste0("xdg-open ", path))
  } else {
    stop("Open browser is not implemented for your system (",
         si, ") in this package (due to incompetence of the author).")
  }
  return(invisible(NULL))
}
