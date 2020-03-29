#' Replace NAs with modus
#'
#' Two possibilities:
#' 1. If "var" is defined as a variable name (character):
#' All NA values of this variable will be replaced with modus of that variable.
#' 2. If no "var" is defined:
#' All character and factor variables in the data.frame are examined.
#' In both cases a new variable will be created that retains information about the
#' replaced missing values.
#' If "use_other_df" is provided, then the modus is calculated on that data.frame
#' (for example calculated on training data and imputed on test data).
#'
#' @param df data.frame
#' @param var character (optional)
#' @param use_other_df data.frame
#'
#' @return Boolean value TRUE or FALSE
#'
#' @examples
#' SR_replace_NA_modus(df = data.frame(a = factor(c(1, 2, 2, 3, NA))),
#'                     var = "a")
#' SR_replace_NA_modus(df = data.frame(a = factor(c(1, 2, 2, 3, NA)),
#'                                     b = c("a", NA, "b", "c", "c")),
#'                     use_other_df = data.frame(a = factor(c(4, 5, 5, 6, 7)),
#'                                               b = c("e", "f", "X", "X", "h")))
#'
#' @export
SR_replace_NA_modus <- function(df, var = NULL, use_other_df = NULL) {
  library(forcats)
  # if other_df is defined, then use other_df for calculation of median
  if (is.null(use_other_df)) use_other_df <- df
  # if single var
  if (!is.null(var)) {
    # create a new variable that retains the information about missing values
    df[, paste0(var, "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
    # replace NAs with modus
    df[is.na(df[, var]), var] <- SR_modus(use_other_df[, var])
  } else {
    for (var in which(sapply(df, is.factor))) {
      if (sum(is.na(df[, var])) > 0) {
        # create a new variable that retains the information about missing values
        df[, paste0(names(df)[var], "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
        # replace NAs with modus, check if level already exists and expand levels if not
        na_value <- SR_modus(use_other_df[, var])
        if (!na_value %in% levels(df[, var])) {
          df[, var] <- forcats::fct_expand(df[, var], SR_modus(use_other_df[, var]))
        }
        df[is.na(df[, var]), var] <- na_value
      }
    }
    for (var in which(sapply(df, is.character))) {
      if (sum(is.na(df[, var])) > 0) {
        # create a new variable that retains information about missing values
        df[, paste0(names(df)[var], "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
        # replace NAs with modus
        df[is.na(df[, var]), var] <- SR_modus(use_other_df[, var])
      }
    }
  }
  return(df)
}
