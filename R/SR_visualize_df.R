SR_visualize_df <- function(df, sample = 1000, save = FALSE, df_name = "", no_graphs = 6) {
  p_load(purrr, tidyr)
  if (save) {
    if (!dir.exists(path_output)) dir.create(path_output)
  }
  if (nrow(df) > sample) {
    set.seed(12345)
    df <- df %>% sample_n(sample)
  }
  try({
    temp <- df %>% purrr::keep(is.numeric)
    if (nrow(temp) > 0 & ncol(temp) > 0) {
      for (i in 1:ceiling(ncol(temp) / no_graphs)) {
        if (length(((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))) == 1) {
          temp_ <- temp %>% gather()
        } else {
          temp_ <- temp[, ((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))] %>%
            gather()
        }
        p <- temp_ %>%
          ggplot(aes(value)) +
          facet_wrap(~ key, scales = "free") +
          geom_histogram(bins = 30, na.rm = TRUE) +
          labs(title = "Numeric variables", x = "", y = "Anzahl")
        print(p)
        if (save) ggsave(paste0(path_output, df_name, " Numeric variables ", i, ".png"), plot = p,
                         width = 9.92, height = 5.3)
        rm(temp_, p)
      }; rm(i)
    }
    rm(temp)
  }, TRUE)
  try({
    temp <- df %>% purrr::keep(is.factor)
    if (nrow(temp) > 0 & ncol(temp) > 0) {
      for (i in 1:ceiling(ncol(temp) / no_graphs)) {
        if (length(((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))) == 1) {
          temp_ <- temp %>% gather()
        } else {
          temp_ <- temp[, ((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))] %>%
            gather()
        }
        p <- temp_ %>%
          ggplot(aes(value)) +
          facet_wrap(~ key, scales = "free") +
          geom_bar(na.rm = TRUE) +
          labs(title = "Factor variables", x = "", y = "Anzahl")
        print(p)
        if (save) ggsave(paste0(path_output, df_name, " Factor variables ", i, ".png"), plot = p,
                         width = 9.92, height = 5.3)
        rm(temp_, p)
      }; rm(i)
    }
    rm(temp)
  }, TRUE)
  try({
    temp <- df %>% purrr::keep(is.character)
    if (nrow(temp) > 0 & ncol(temp) > 0) {
      for (i in 1:ceiling(ncol(temp) / no_graphs)) {
        if (length(((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))) == 1) {
          temp_ <- temp %>% gather()
        } else {
          temp_ <- temp[, ((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))] %>%
            gather()
        }
        p <- temp_ %>%
          ggplot(aes(value)) +
          facet_wrap(~ key, scales = "free") +
          geom_bar(na.rm = TRUE) +
          labs(title = "Character variables", x = "", y = "Anzahl")
        print(p)
        if (save) ggsave(paste0(path_output, df_name, " Character variables ", i, ".png"), plot = p,
                         width = 9.92, height = 5.3)
        rm(temp_, p)
      }; rm(i)
    }
    rm(temp)
  }, TRUE)
  try({
    temp <- df %>% purrr::keep(SR_is_date)
    if (nrow(temp) > 0 & ncol(temp) > 0) {
      for (i in 1:ceiling(ncol(temp) / no_graphs)) {
        if (length(((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))) == 1) {
          temp_ <- temp %>% gather()
        } else {
          temp_ <- temp[, ((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))] %>%
            data.frame() %>%
            gather()
        }
        p <- temp_ %>%
          ggplot(aes(value)) + facet_wrap(~ key, scales = "free") +
          geom_histogram(bins = 30, na.rm = TRUE) +
          labs(title = "Date variables", x = "", y = "Anzahl")
        print(p)
        if (save) ggsave(paste0(path_output, df_name, " Date variables ", i, ".png"), plot = p,
                         width = 9.92, height = 5.3)
        rm(temp_, p)
      }; rm(i)
    }
    rm(temp)
  }, TRUE)
}
