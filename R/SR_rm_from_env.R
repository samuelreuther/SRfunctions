#' Remove objects from global environment by pattern
#'
#' Removes all objects whose names match a regular expression from the global
#' environment.
#'
#' @param pattern character: regular expression passed to \code{ls()}
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' SR_rm_from_env(pattern = "^tmp_")
#' }
#'
#' @export
SR_rm_from_env <- function(pattern) {
  rm(list = ls(pattern = pattern, envir = .GlobalEnv), envir = .GlobalEnv)
}
