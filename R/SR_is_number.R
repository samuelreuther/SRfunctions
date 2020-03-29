SR_is_number <- function(x) {
  ifelse(is.numeric(x) | is.integer(x) | SR_is_date(x), T, F)
}
