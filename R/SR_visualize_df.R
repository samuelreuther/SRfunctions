#' Visualize data.frame
#'
#' Plots all variables of a data.frame in groups of their classes (numeric,
#' factor/character, date)
#'
#' @param df data.frame
#' @param sample integer
#' @param no_graphs integer
#' @param save Boolean
#' @param df_name character, must end with "*png"
#'
#' @return NULL, prints plots
#'
#' @examples
#' SR_visualize_df(data.frame(var_numeric = c(1, 2, 2, 3, 3, 4),
#'                            var_character = c("a", NA, "b", "c", "c", "d"),
#'                            var_factor = factor(c("a", "b", "c", "c", "d", NA)),
#'                            var_date = seq.Date(Sys.Date(), by = "day", length.out = 6),
#'                            stringsAsFactors = FALSE))
#'
#' @export
SR_visualize_df <- function(df,
                            sample = 1000,
                            no_graphs = 6,
                            save = FALSE,
                            df_name = "") {
  if (save) {
    if (!dir.exists(path_output)) dir.create(path_output)
  }
  if (nrow(df) > sample) {
    set.seed(12345)
    df <- df %>% dplyr::sample_n(sample)
  }
  try({
    temp <- df %>% purrr::keep(is.numeric)
    if (nrow(temp) > 0 & ncol(temp) > 0) {
      for (i in 1:ceiling(ncol(temp) / no_graphs)) {
        if (length(((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))) == 1) {
          temp_ <- temp %>% tidyr::gather()
        } else {
          temp_ <- temp[, ((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))] %>%
            tidyr::gather()
        }
        p <- temp_ %>%
          ggplot2::ggplot(ggplot2::aes(value)) +
          ggplot2::facet_wrap(~ key, scales = "free") +
          ggplot2::geom_histogram(bins = 30, na.rm = TRUE) +
          ggplot2::labs(title = "Numeric variables", x = "", y = "Anzahl")
        print(p)
        if (save) ggplot2::ggsave(paste0(path_output, df_name, " Numeric variables ", i, ".png"),
                                  plot = p, width = 9.92, height = 5.3)
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
          temp_ <- temp %>% tidyr::gather()
        } else {
          temp_ <- temp[, ((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))] %>%
            tidyr::gather()
        }
        p <- temp_ %>%
          ggplot2::ggplot(ggplot2::aes(value)) +
          ggplot2::facet_wrap(~ key, scales = "free") +
          ggplot2::geom_bar(na.rm = TRUE) +
          ggplot2::labs(title = "Factor variables", x = "", y = "Anzahl")
        print(p)
        if (save) ggplot2::ggsave(paste0(path_output, df_name, " Factor variables ", i, ".png"),
                                  plot = p, width = 9.92, height = 5.3)
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
          temp_ <- temp %>% tidyr::gather()
        } else {
          temp_ <- temp[, ((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))] %>%
            tidyr::gather()
        }
        p <- temp_ %>%
          ggplot2::ggplot(ggplot2::aes(value)) +
          ggplot2::facet_wrap(~ key, scales = "free") +
          ggplot2::geom_bar(na.rm = TRUE) +
          ggplot2::labs(title = "Character variables", x = "", y = "Anzahl")
        print(p)
        if (save) ggplot2::ggsave(paste0(path_output, df_name, " Character variables ", i, ".png"),
                                  plot = p, width = 9.92, height = 5.3)
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
          temp_ <- temp %>% tidyr::gather()
        } else {
          temp_ <- temp[, ((i - 1) * no_graphs + 1):min(i * no_graphs, ncol(temp))] %>%
            data.frame() %>%
            tidyr::gather()
        }
        p <- temp_ %>%
          ggplot2::ggplot(ggplot2::aes(value)) +
          ggplot2::facet_wrap(~ key, scales = "free") +
          ggplot2::geom_histogram(bins = 30, na.rm = TRUE) +
          ggplot2::labs(title = "Date variables", x = "", y = "Anzahl")
        print(p)
        if (save) ggplot2::ggsave(paste0(path_output, df_name, " Date variables ", i, ".png"),
                                  plot = p, width = 9.92, height = 5.3)
        rm(temp_, p)
      }; rm(i)
    }
    rm(temp)
  }, TRUE)
  #
  # return NULL
  return(invisible(NULL))
}
