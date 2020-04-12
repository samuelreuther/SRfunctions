#' Check if object is a number
#'
#' check if object is.numeric(), is.integer() or SR_is_date()
#'
#' @param x object
#'
#' @return Boolean value TRUE or FALSE
#'
#' @examples
#' SR_is_number(42.2)
#'
#' @export
SR_is_number <- function(x) {
  ifelse(is.numeric(x) | is.integer(x) | SR_is_date(x), T, F)
}
