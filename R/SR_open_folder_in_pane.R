#' Open folder in the IDE file pane
#'
#' Shows a folder in the Files pane of RStudio / RStudio Server or in the
#' Explorer of Positron.
#'
#' Positron cannot navigate its Explorer to a folder directly. The function
#' focuses the Explorer and opens the first file of the folder, so the
#' Explorer reveals the folder. This works only for folders inside the current
#' workspace. For empty folders or folders outside the workspace, the
#' function only focuses the Explorer.
#'
#' @param path Path to an existing folder. Default is the working directory.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' SR_open_folder_in_pane("R")
#' }
#'
#' @export
SR_open_folder_in_pane <- function(path = getwd()) {
  if (!dir.exists(path)) {
    stop("Folder does not exist: ", path, call. = FALSE)
  }
  path <- normalizePath(path, winslash = "/")

  if ("tools:positron" %in% search()) {
    positron_env <- as.environment("tools:positron")
    positron_env$.ps.ui.executeCommand("workbench.view.explorer")

    workspace <- normalizePath(positron_env$.ps.ui.workspaceFolder(), winslash = "/")
    files <- list.files(path, full.names = TRUE)
    files <- files[!dir.exists(files)]
    if (!startsWith(tolower(path), tolower(workspace)) || length(files) == 0) {
      message("Positron: folder outside workspace or without files, only Explorer focused.")
      return(invisible(NULL))
    }
    positron_env$.ps.ui.navigateToFile(files[1])
    return(invisible(NULL))
  }

  if (!rstudioapi::isAvailable()) {
    stop("Needs RStudio, RStudio Server or Positron.", call. = FALSE)
  }
  try(rstudioapi::executeCommand("activateFiles"), TRUE)
  Sys.sleep(0.2)
  try(rstudioapi::filesPaneNavigate(path), TRUE)
  invisible(NULL)
}
