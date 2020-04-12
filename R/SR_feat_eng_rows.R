#' Feature engineering for rows
#'
#' Lots of common feature engineering utilities. Currently most of the KPIs support
#' only numeric variables.
#'
#' @param df data.frame
#' @param row_count_na boolean (default = TRUE): all variables
#' @param row_count_zero boolean (default = TRUE): numeric values if > 1
#' @param row_count_negative boolean (default = TRUE): numeric values if > 1
#' @param row_min boolean (default = TRUE): numeric values if > 1
#' @param row_mean boolean (default = TRUE): numeric values if > 1
#' @param row_max boolean (default = TRUE): numeric values if > 1
#'
#' @return data.frame
#'
#' @examples
#' df <- data.frame(var_numeric1 = c(-1, 0, 2, 3, NA, 4),
#'                  var_numeric2 = c(-5, 0, -2, NA, 3, 4),
#'                  var_character = c("a", NA, "b", "c", "c", "d"),
#'                  var_factor = factor(c("a", "b", "c", "c", "d", NA)),
#'                  var_date = seq.Date(as.Date("2020-04-12"), by = "day", length.out = 6),
#'                  stringsAsFactors = FALSE)
#' SR_feat_eng_rows(df)
#' rm(df)
#'
#' @export
SR_feat_eng_rows <- function(df,
                             row_count_na = TRUE,
                             row_count_zero = TRUE,
                             row_count_negative = TRUE,
                             row_min = TRUE,
                             row_mean = TRUE,
                             row_max = TRUE) {
  # make copy of df
  df_final <- df
  #
  # row_count_na
  if (row_count_na) {
    df_final$row_count_na <- rowSums(is.na(df))
  }
  #
  # detect numeric columns
  numeric_columns <- sapply(df, is.numeric)
  # numeric_columns <- sapply(df, SR_is_number)
  #
  # calculate KPIs over numeric columns
  if (sum(numeric_columns) > 1) {
    # row_count_zero
    if (row_count_zero) {
      df_final$row_count_zero <- rowSums(df[, numeric_columns] == 0, na.rm = TRUE)
    }
    #
    # row_count_negative
    if (row_count_negative) {
      df_final$row_count_negative <- rowSums(df[, numeric_columns] < 0, na.rm = TRUE)
    }
    #
    # row_min
    if (row_min) {
      df_final$row_min <- apply(df[, numeric_columns], 1, min, na.rm = TRUE)
    }
    #
    # row_mean
    if (row_mean) {
      df_final$row_mean <- apply(df[, numeric_columns], 1, mean, na.rm = TRUE)
      # df_final$row_mean <- rowMeans(df[, numeric_columns], na.rm = TRUE)
    }
    #
    # row_max
    if (row_max) {
      df_final$row_max <- apply(df[, numeric_columns], 1, max, na.rm = TRUE)
    }
  }
  #
  # return results
  return(df_final)
}
