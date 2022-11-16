#' Feature engineering for variables of class "numeric"
#'
#' Lots of common feature engineering utilities.
#'
#' @param df data.frame
#' @param trim_outliers boolean (default = FALSE): limit = 5 * standard deviation
#' @param replace_NA_special_value boolean (default = FALSE): trying to find a special
#'                                 value like -1, 1, or 999*
#' @param replace_NA_median boolean (default = FALSE): alternative NA treatment,
#'                          creates a separate column to flag NA existence
#' @param log_scale_p1 boolean (default = FALSE): log_scale, i.e. if variable is skewed
#' @param interactions boolean (default = FALSE): calculate variables with interactions
#'                     with "_plus_", "_multi_", "_minus_", "_div_"
#' @param folds_index boolean (default = NULL): not implemented atm
#' @param exception character (default = NULL): except variables from feature engineering
#' @param use_other_df character (default = NULL): use other provided data.frame
#'                     for calculating statistics
#'
#' @return data.frame
#'
#' @examples
#' df <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 4),
#'                  var_character = c("a", NA, "b", "c", "c", "d"),
#'                  var_factor = factor(c("a", "b", "c", "c", "d", NA)),
#'                  var_date = seq.Date(as.Date("2020-04-12"), by = "day", length.out = 6),
#'                  stringsAsFactors = FALSE)
#' SR_feat_eng_date(df)
#' SR_feat_eng_date(df, only_date_to_numeric = FALSE)
#' rm(df)
#'
#' @export
SR_feat_eng_numeric <- function(df,
                                trim_outliers = FALSE,
                                replace_NA_special_value = FALSE,
                                replace_NA_median = FALSE,
                                log_scale_p1 = FALSE,
                                interactions = FALSE,
                                folds_index = NULL,
                                exception = NULL,
                                use_other_df = NULL) {
  if (class(df)[1] != "data.frame") df <- data.frame(df)
  for (i in 1:ncol(df)) {
    if (is.numeric(df[, i]) | sapply(df[, i], class)[[1]] == "numeric") {
      j <- names(df)[i]
      if (j %in% exception) next()
      #
      # trim outliers
      if (trim_outliers & !grepl("_LabelEnc", j)) {
        limit = 5
        if (is.null(use_other_df)) use_other_df <- df
        mean <- mean(use_other_df[, j] %>% SR_omit_non_regular_values(), na.rm = TRUE)
        sd <- stats::sd(use_other_df[, j] %>% SR_omit_non_regular_values(), na.rm = TRUE)
        if (!mean %in% c(Inf, NA, NaN) & !sd %in% c(Inf, NA, NaN)) {
          df[!is.na(df[, j]) & df[, j] > mean + limit * sd, j] <- mean + limit * sd
          df[!is.na(df[, j]) & df[, j] < mean - limit * sd, j] <- mean - limit * sd
        }
      }
      #
      # replace_NA_special_value
      if (replace_NA_special_value & sum(is.na(df[, i])) > 0) {
        if (min(df[, j], na.rm = TRUE) >= 0) {
          df[is.na(df[, j]), j] <- -1
        } else {
          if ((max(df[, j], na.rm = TRUE) <= 0)) {
            df[is.na(df[, j]), j] <- 1
          } else {
            df[is.na(df[, j]), j] <- as.numeric(paste0(replicate(
              nchar(floor(max(df[, j], na.rm = TRUE))) + 1, 9), collapse = ""))
          }
        }
      }
      #
      # replace_NA_median
      # => out of fold !!!     # TODO !!!
      if (replace_NA_median & sum(is.na(df[, i])) > 0) {
        # df[is.na(df[, j]), j] <- median(df[, j], na.rm = TRUE)
        df <- SR_replace_NA_median(df, var = j, use_other_df = use_other_df)
      }
      #
      # log_scale, i.e. if variable is skewed
      if (log_scale_p1 & !grepl("_LabelEnc", j)) {
        # pacman::p_load(moments)
        if (moments::skewness(df[, j]) > 2) {
          df[, ] <- log(df[, j] + 1)
        }
      }
      #
      # interactions
      if (interactions) {
        for (k in 1:ncol(df)) {
          if (is.numeric(df[, k]) & k != i) {
            l <- names(df)[k]
            if (i > k) {
              df[, paste0(j, "_plus_", l)] <- df[, j] + df[, l]
              df[, paste0(j, "_multi_", l)] <- df[, j] * df[, l]
            }
            df[, paste0(j, "_minus_", l)] <- df[, j] - df[, l]
            df[, paste0(j, "_div_", l)] <- df[, j] / df[, l]
          }; rm(l)
        }; rm(k)
      }
      rm(j)
    }
  }
  rm(i)
  return(df)
}
