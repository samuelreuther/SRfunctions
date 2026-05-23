#' Open GitHub repository
#'
#' Opens the GitHub repository of the current project in the browser.
#'
#' @return None
#'
#' @examples
#' \dontrun{
#' SR_open_github()
#' }
#'
#' @export
SR_open_github <- function() {
  usethis::browse_github()
}
