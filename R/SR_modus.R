SR_modus <- function(x) {
  # fastest
  names(sort(table(x), decreasing = T)[1])
  #
  # Alternative 1
  # names(which.max(table(x)))
  #
  # Alternative 2
  # ux <- n_distinct(x)
  # ux <- unique(x)
  # ux[which.max(tabulate(match(x, ux)))]
}
