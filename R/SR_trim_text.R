#' Remove leading or trailing whitespace from text
#'
#' removes leading or trailing whitespace from text
#'
#' @param x text or character variable with text
#'
#' @return text or character variable with text
#'
#' @examples
#' SR_trim_text(" abc ")
#'
#' @export
SR_trim_text <- function(x) {
  gsub("^\\s+|\\s+$", "", x)
}
