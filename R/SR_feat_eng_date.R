SR_feat_eng_date <- function(df, only_date_to_numeric = TRUE) {
  for (i in 1:ncol(df)) {
    if (SR_is_date(df[, i])) {
      j <- names(df)[i]
      if (!only_date_to_numeric) {
        # df[, paste0(j, "_year")] <- year(df[, j])
        df[, paste0(j, "_month")] <- lubridate::month(df[, j])
        df[, paste0(j, "_day")] <- as.numeric(format(df[, j], format = "%d"))
        df[, paste0(j, "_weekday")] <- as.factor(weekdays(df[, j], abbreviate = FALSE))
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
