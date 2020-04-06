SR_feat_eng_rows <- function(df,
                             na_count = FALSE,
                             zero_count = FALSE,
                             negative_count = FALSE,
                             row_mean = FALSE,
                             row_max = FALSE,
                             row_min = FALSE) {
  for (i in 1:ncol(df)) {
    if (is.numeric(df[, i])) {
      j <- names(df)[i]
      #
      # na_count
      if (na_count) {
        df$na_count <- rowSums(is.na(df))
        df$na_count <- df$na_count + rowSums(df == "Missing")
      }
      #
      # zero_count
      if (zero_count) {
        df$zero_count <- rowSums(df == 0, na.rm = TRUE)
      }
      #
      # negative_count
      if (negative_count) {
        df$negative_count <- rowSums(df < 0, na.rm = TRUE)
      }
      #
      # row_mean
      if (row_mean) {
        df$row_mean <- rowMeans(df[, sapply(df, is.numeric)], na.rm = TRUE)
      }
      #
      # row_max
      if (row_max) {
        df$row_max <- apply(df[, sapply(df, is.numeric)], 1, max, na.rm = TRUE)
      }
      #
      # row_min
      if (row_min) {
        df$row_min <- apply(df[, sapply(df, is.numeric)], 1, min, na.rm = TRUE)
      }
      #
    }; rm(j)
  }; rm(i)
  return(df)
}
