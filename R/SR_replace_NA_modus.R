SR_replace_NA_modus <- function(df, var = NULL, use_other_df = NULL) {
  # if other_df is defined, then use other_df for calculation of median
  if (is.null(use_other_df)) use_other_df <- df
  # if single var
  if (!is.null(var)) {
    # create a new variable that retains information about missing values
    df[, paste0(var, "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
    # replace NAs with modus
    df[is.na(df[, var]), var] <- SR_modus(df[, var])
  } else {
    for (var in which(sapply(df, is.factor))) {
      if (sum(is.na(df[, var])) > 0) {
        # create a new variable that retains information about missing values
        df[, paste0(var, "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
        # replace NAs with modus
        df[is.na(df[, var]), var] <- SR_modus(df[, var])
      }
    }
    for (var in which(sapply(df, is.character))) {
      if (sum(is.na(df[, var])) > 0) {
        # create a new variable that retains information about missing values
        df[, paste0(var, "_NA")] <- ifelse(is.na(df[, var]), 1, 0)
        # replace NAs with modus
        df[is.na(df[, var]), var] <- SR_modus(df[, var])
      }
    }
  }
  return(df)
}
