SR_feat_eng_factors <- function(df,
                                order_weekday_month = TRUE,
                                make_na_explicit = FALSE,
                                replace_na_by_modus = FALSE,
                                label_encoding = FALSE,
                                count_encoding = FALSE,
                                rank_encoding = FALSE,
                                # weighted_effect_coding = FALSE,
                                target_encoding = FALSE,
                                polynomial_encoding = FALSE,
                                combine_rare_levels = FALSE, prop = NULL,
                                folds_index = NULL,
                                use_other_df = NULL) {
  #
  # https://www.slideshare.net/HJvanVeen/feature-engineering-72376750
  #
  ### initiate table for saving encodings
  if (label_encoding & is.null(use_other_df)) {
    factor_encoding <- data.frame(feature = NA, no = NA, levels = NA)
    factor_encoding <- na.omit(factor_encoding)
  }
  #
  for (i in names(df)) {
    if (is.factor(df[, i]) | is.character(df[, i])) {
      #
      ### order_weekday_month in natural order
      if (order_weekday_month) {
        # weekday german
        wd <- c("Montag", "Dienstag", "Mittwoch", "Donnerstag", "Freitag",
                "Samstag", "Sonntag")
        if (sum(tolower(unique(df[, i])) %in% tolower(wd)) > 0) {
          df[, i] <- factor(df[, i], levels = wd)
        }; rm(wd)
        # weekday english
        wd <- c("monday", "tuesday", "wednesday", "thursday", "friday",
                "saturday", "sunday")
        if (sum(tolower(unique(df[, i])) %in% tolower(wd)) > 0) {
          df[, i] <- factor(df[, i], levels = wd)
        }; rm(wd)
        # month german
        wd <- c("Januar", "Februar", "Maerz", "April", "Mai", "Juni",
                "Juli", "August", "September", "Oktober", "November", "Dezember")
        if (sum(tolower(unique(df[, i])) %in% tolower(wd)) > 0) {
          df[, i] <- factor(df[, i], levels = wd)
        }; rm(wd)
        # month english
        wd <- c("january", "february", "march", "april", "may", "june",
                "july", "august", "september", "october", "november", "december")
        if (sum(tolower(unique(df[, i])) %in% tolower(wd)) > 4) {
          df[, i] <- factor(df[, i], levels = wd)
        }; rm(wd)
      }
      #
      ### make_na_explicit
      if (make_na_explicit & sum(is.na(df[, i])) > 0) {
        df[, i] <- fct_explicit_na(df[, i], na_level = "Missing")
      }
      #
      ### replace_na_by_modus
      if (replace_na_by_modus & sum(is.na(df[, i])) > 0) {
        df[is.na(df[, i]), i] <- SR_modus(df[, i])
      }
      #
      # combine_rare_levels
      # => out of fold !!!     # TODO !!!
      if (combine_rare_levels) {
        if (is.null(use_other_df)) {
          if (is.null(prop)) {
            df[, i] <- fct_lump(df[, i], other_level = "Other")
            # suppressWarnings(df[, i] <- fct_lump(df[, i], other_level = "Other"))   # "other" is still the smallest level
          } else {
            df[, i] <- fct_lump(df[, i], prop = prop, other_level = "Other")
            # suppressWarnings(df[, i] <- fct_lump(df[, i], prop = prop, other_level = "Other"))
          }
        } else {
          # convert to factor with levels from other df
          df[, i] <- factor(df[, i], levels = levels(use_other_df[, i]))
          if (sum(is.na(df[, i])) > 0) {
            suppressWarnings(df[, i] <- fct_other(df[, i], keep = levels(df[, i]),
                                                  other_level = "Other"))
            # df[, i] <- fct_other(df[, i], keep = levels(df[, i]), other_level = "Other")
            df[, i] <- fct_explicit_na(df[, i], na_level = "Other")
            # suppressWarnings(df[, i][is.na(df[, i])] <- "Other")
          }
        }
      }
      #
      ### label_encoding
      if (label_encoding) {
        # label encoding
        if (is.null(use_other_df)) {
          # convert to factor
          if (is.character(df[, i])) df[, i] <- as.factor(df[, i])
          # save encoding
          factor_encoding <- rbind(factor_encoding,
                                   data.frame(feature = i,
                                              no = 1:length(levels(df[, i])),
                                              levels = levels(df[, i])))
          df[, i] <- as.numeric(df[, i])
          # colnames(df)[colnames(df) == i] <- paste0(i, "_LabelEnc")
          setnames(df, i, paste0(i, "_LabelEnc"))
          # df[, paste0(i, "_LabelEnc")] <- as.numeric(df[, i])
        } else {
          # use_other_df
          # convert to factor with levels from other df
          df[, i] <- factor(df[, i],
                            levels = factor_encoding$levels[factor_encoding$feature == i])
          if (sum(is.na(df[, i])) > 0) {
            suppressWarnings(df[, i] <- fct_other(df[, i], keep = levels(df[, i]),
                                                  other_level = "Other"))
            df[, i] <- fct_explicit_na(df[, i], na_level = "Other")
          }
          # if (is.character(df[, i])) {
          #   df[, i] <- factor(df[, i], levels = levels(use_other_df[, i]))
          # }
          df[, i] <- as.numeric(df[, i])
          # colnames(df)[colnames(df) == i] <- paste0(i, "_LabelEnc")
          # setnames(df, gsub("_LabelEnc_LabelEnc", "_LabelEnc", names(df)))
          setnames(df, i, paste0(i, "_LabelEnc"))
          # setnames(df, gsub("_LabelEnc_LabelEnc", "_LabelEnc", names(df)))
          # df[, paste0(i, "_LabelEnc")] <- as.numeric(df[, i])
        }
        # df[, i] <- NULL   # ???
      }
      #
      # ### count_encoding
      # # => out of fold !!!      # TODO !!!
      # if (count_encoding) {
      #   temp <- count(df, df[, i]) %>% data.frame %>% setnames(c("level", "n", "rank"))
      #   # assign(temp)
      #   df[, paste0(i, "_CountEnc")]
      #   for (k in 1:length(levels(df[, i]))) {
      #     df[, paste0(i, "_CountEnc")][df[, i] == temp$level[k]] <- temp$n[k]
      #   }
      #   df[, i] <- NULL
      #   rm(k, temp)
      # }
      #
      # # rank_encoding
      # # => out of fold !!!     # TODO !!!
      # if (rank_encoding) {
      #   temp <- count(df, df[, i]) %>% mutate(rank = rank(-n)) %>% data.frame %>%
      #     setnames(c("level", "n", "rank"))
      #   # assign(temp)
      #   df[, paste0(i, "_RankEnc")] <- NA
      #   for (k in 1:length(levels(df[, i]))) {
      #     df[, paste0(i, "_RankEnc")][df[, i] == temp$level[k]] <- temp$rank[k]
      #   }
      #   df[, i] <- NULL
      #   rm(k, temp)
      # }
      #
      # # weighted_effect_coding
      # # https://link.springer.com/article/10.1007/s00038-016-0901-1
      # => out of fold !!!
      # if (weighted_effect_coding) {
      #   p_load(wec)
      #   omitted <- names(sort(table(df[, var]), decreasing = TRUE)[1])
      #   temp <- contr.wec(df[, var], omitted = omitted)
      #   for (k in 1:ncol(temp)) {
      #     df[df[, i] == temp$df...i.[k], paste0(i, "_wec")] <- temp$rank[k]  # TODO
      #   }; rm(k, temp, omitted)
      # }
      #
      # # target_encoding => very good, if done right!
      # # => out of fold !!!     # TODO !!!
      # # add smoothing to avoid 0!
      # # add random noise to avoid overfit!
      # if (target_encoding) {
      #   # ...
      # }
      #
      # polynomial_encoding
      # A = 1 * B = 1 ...
      if (polynomial_encoding) {
        for (k in 1:ncol(df)) {
          if (is.factor(df[, k]) | is.character(df[, k]) & i > k) {
            l <- names(df)[k]
            df[, paste0(i, "_X_", l)] <- paste0(df[, i], "_X_", df[, l])
          }; rm(l)
        }; rm(k)
      }
    }
  }
  rm(i)
  if (label_encoding) {
    assign("factor_encoding", factor_encoding, envir = .GlobalEnv)
  }
  df <- droplevels(df)
  return(df)
}
