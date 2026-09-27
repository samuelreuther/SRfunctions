#' Open folder in Windows Explorer
#'
#' Opens a Windows Explorer window of a folder. Works only on Windows.
#'
#' @param path Path to an existing folder. Default is the working directory.
#'   Windows paths with backslashes need a raw string, e.g.
#'   \code{r"(C:\\temp)"}, otherwise R fails already when parsing.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' SR_open_folder_in_explorer("R")
#' SR_open_folder_in_explorer(r"(C:\home\sandbox\sandbox\Libraries)")
#' }
#'
#' @export
SR_open_folder_in_explorer <- function(path = getwd()) {
  if (.Platform$OS.type != "windows") {
    stop("Works only on Windows.", call. = FALSE)
  }
  if (!dir.exists(path)) {
    stop("Folder does not exist: ", path, call. = FALSE)
  }
  path <- normalizePath(path)
  shell.exec(path)
  # rsession runs in background, so Windows opens the window behind RStudio
  system2(
    "powershell",
    c("-NoProfile", "-File",
      shQuote(system.file("SR_bring_to_front.ps1", package = "SRfunctions"), type = "cmd"),
      "-Path", shQuote(path, type = "cmd")),
    wait = FALSE,
    invisible = TRUE
  )
  invisible(NULL)
}
