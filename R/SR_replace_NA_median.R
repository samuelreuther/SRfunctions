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
      df[is.na(df[, var]), var] <- median(use_other_df[, var], na.rm = T)
    }
  } else {
    # for the whole data.frame
    for (var in which(sapply(df, SR_is_number))) {
      if (sum(is.na(df[, var])) > 0) {
        # create a new variable that retains information about missing values
        df[, paste0(var, "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
        # replace NAs with median
        df[is.na(df[, var]), var] <- median(use_other_df[, var], na.rm = T)
      }
    }
  }
  return(df)
}
