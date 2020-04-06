#' Calculate modus
#'
#' calculate most frequent value in a vector
#'
#' @param x vector
#'
#' @return character
#'
#' @examples
#' SR_modus(c("a", "a", "b", "c"))
#'
#' @export
SR_modus <- function(x) {
  # fastest
  names(sort(table(x), decreasing = TRUE)[1])
  #
  # Alternative 1
  # names(which.max(table(x)))
  #
  # Alternative 2
  # ux <- n_distinct(x)
  # ux <- unique(x)
  # ux[which.max(tabulate(match(x, ux)))]
}
