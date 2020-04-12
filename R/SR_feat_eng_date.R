#' Feature engineering for variables of class "Date"
#'
#' class "Date" is determined by function SR_is_date()
#'
#' @param df data.frame
#' @param only_date_to_numeric boolean (default = TRUE): if FALSE then in addition
#' the month, day and weekday of date-variables are added as new columns
#'
#' @return Boolean value TRUE or FALSE
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
SR_feat_eng_date <- function(df, only_date_to_numeric = TRUE) {
  for (i in 1:ncol(df)) {
    if (SR_is_date(df[, i])) {
      j <- names(df)[i]
      if (!only_date_to_numeric) {
        # df[, paste0(j, "_year")] <- year(df[, j])
        df[, paste0(j, "_month")] <- lubridate::month(df[, j])
        df[, paste0(j, "_day")] <- as.numeric(format(df[, j], format = "%d"))
        df[, paste0(j, "_weekday")] <- weekdays(df[, j], abbreviate = FALSE)
        # df[, paste0(j, "_year_month")] <- lubridate::month(df[, j]) + lubridate::year(df[, j]) * 100
        # as.factor(paste0(df[, paste0(j, "_year")], "_",
        #                  df[, paste0(j, "_month")]))
        # df[, paste0(j, "_year_week")] <- lubridate::week(df[, j]) + lubridate::year(df[, j]) * 100
      }
      df[, j] <-
        lubridate::year(df[, j]) * 10000 +
        lubridate::month(df[, j]) * 100 +
        as.numeric(format(df[, j], "%d"))
      # df[, paste0(j, "_numeric")] <- as.numeric(df[, j])
      # df[, j] <- NULL
    }
  } # ; rm(i, j)
  return(df)
}
