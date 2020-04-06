#' Replace NAs with median
#'
#' Two possibilities:
#' 1. If "var" is defined as a variable name (character):
#' All NA values of this variable will be replaced with the median of that variable.
#' 2. If no "var" is defined:
#' All numeric variables in the data.frame are examined.
#' In both cases a new variable will be created that retains information about the
#' replaced missing values.
#' If "use_other_df" is provided, then the median is calculated on that data.frame
#' (for example calculated on training data and imputed on test data).
#'
#' @param df data.frame
#' @param var character (optional)
#' @param use_other_df data.frame
#'
#' @return Boolean value TRUE or FALSE
#'
#' @examples
#' SR_replace_NA_median(df = data.frame(a = c(1, 2, 2, 3, NA)),
#'                      var = "a")
#' SR_replace_NA_median(df = data.frame(a = c(1, 2, 2, 3, NA)),
#'                      use_other_df = data.frame(a = c(4, 5, 5, 6, 7)))
#'
#' @export
SR_replace_NA_median <- function(df, var = NULL, use_other_df = NULL) {
  # if other_df is defined, then use other_df for calculation of median
  if (is.null(use_other_df)) use_other_df <- df
  # replace NAs with median
  if (!is.null(var)) {
    # for a specific variable
    if (sum(is.na(df[, var])) > 0) {
      # create a new variable that retains information about missing values
      df[, paste0(var, "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
      # replace NAs with median
      df[is.na(df[, var]), var] <- median(use_other_df[, var], na.rm = TRUE)
    }
  } else {
    # for the whole data.frame
    for (var in which(sapply(df, SR_is_number))) {
      if (sum(is.na(df[, var])) > 0) {
        # create a new variable that retains information about missing values
        df[, paste0(names(df)[var], "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
        # replace NAs with median
        df[is.na(df[, var]), var] <- median(use_other_df[, var], na.rm = TRUE)
      }
    }
  }
  return(df)
}
