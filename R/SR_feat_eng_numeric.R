SR_feat_eng_numeric <- function(df,
                                trim_outliers = F,
                                replace_NA_special_value = F,
                                replace_NA_median = F,
                                log_scale_p1 = F,
                                interactions = F,
                                folds_index = NULL,
                                exception = NULL,
                                use_other_df = NULL) {
  if (class(df) != "data.frame") df <- data.frame(df)
  for (i in 1:ncol(df)) {
    if (is.numeric(df[, i]) | sapply(df[, i], class)[[1]] == "numeric") {
      j <- names(df)[i]
      if (j %in% exception) next()
      #
      # trim outliers
      if (trim_outliers & !grepl("_LabelEnc", j)) {
        limit = 5
        if (is.null(use_other_df)) use_other_df <- df
        mean <- mean(use_other_df[, j] %>% inf.omit(), na.rm = T)
        sd <- sd(use_other_df[, j] %>% inf.omit(), na.rm = T)
        if (!mean %in% c(Inf, NA, NaN) & !sd %in% c(Inf, NA, NaN)) {
          df[!is.na(df[, j]) & df[, j] > mean + limit * sd, j] <- mean + limit * sd
          df[!is.na(df[, j]) & df[, j] < mean - limit * sd, j] <- mean - limit * sd
        }
      }
      #
      # replace_NA_special_value
      if (replace_NA_special_value & sum(is.na(df[, i])) > 0) {
        if (min(df[, j], na.rm = T) >= 0) {
          df[is.na(df[, j]), j] <- -1
        } else {
          if ((max(df[, j], na.rm = T) <= 0)) {
            df[is.na(df[, j]), j] <- 1
          } else {
            df[is.na(df[, j]), j] <- as.numeric(paste0(replicate(
              nchar(floor(max(df[, j], na.rm = T))) + 1, 9), collapse = ""))
          }
        }
      }
      #
      # replace_NA_median
      # => out of fold !!!     # TODO !!!
      if (replace_NA_median & sum(is.na(df[, i])) > 0) {
        # df[is.na(df[, j]), j] <- median(df[, j], na.rm = T)
        df <- SR_replace_NA_median(df, var = j, use_other_df = use_other_df)
      }
      #
      # log_scale, i.e. if variable is skewed
      if (log_scale_p1 & !grepl("_LabelEnc", j)) {
        p_load(moments)
        if (skewness(df[, j]) > 2) {
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
