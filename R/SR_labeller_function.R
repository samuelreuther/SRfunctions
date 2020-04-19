#' Add labels to a data.frame
#'
#' Add labels to a data.frame
#' source: https://stackoverflow.com/questions/27347548/r-assign-variable-labels-of-data-frame-columns
#'
#' @param df data.frame
#' @param labels list of column names and labels (see example)
#'
#' @return data.frame
#'
#' @examples
#' df <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 4),
#'                  var_character = c("a", NA, "b", "c", "c", "d"),
#'                  stringsAsFactors = FALSE)
#' df <- SR_labeller_function(df,
#'                            c(var_numeric = "a",
#'                              var_character = "b"))
#' Hmisc::contents(df)
#'
#' @export
SR_labeller_function <- function(df, labels) {
  # return(labels[value])
  Hmisc::upData(df, labels = labels)
}
