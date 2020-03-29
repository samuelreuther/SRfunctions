#' Check if object is class date
#'
#' because function is.Date() is missing in base R
#'
#' @param x object
#'
#' @return Boolean value TRUE or FALSE
#'
#' @examples
#' SR_is_date(Sys.Date())
#'
#' @export
SR_is_date <- function(x) {
  inherits(x, 'Date')
}
