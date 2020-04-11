#' Calculate modus
#'
#' calculate most frequent value in a vector
#'
#' @param x vector
#' @param method integer (default = 1): different methods, with being 1 the fastest
#'
#' @return character
#'
#' @examples
#' SR_modus(c("a", "a", "b", "c"))
#'
#' @export
SR_modus <- function(x, method = 1) {
  if (method == 1) {
    # fastest
    modus <- names(sort(table(x), decreasing = TRUE)[1])
  }
  #
  if (method == 2) {
    # Alternative 1
    modus <- names(which.max(table(x)))
  }
  #
  if (method == 3) {
    # Alternative 2
    ux <- unique(x)
    modus <- ux[which.max(tabulate(match(x, ux)))]
  }
  #
  # return result
  return(modus)
}
