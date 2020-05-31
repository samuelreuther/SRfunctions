#' Correlation plot
#'
#' Plot correlation of all numeric, integer and date variables of a data.frame.
#'
#' @param df data.frame
#' @param cor_threshold numeric between 0 and 1
#' @param save Boolean
#' @param filename character, must end with "*png"
#' @param path_output character
#'
#' @return matrix, prints a plot
#'
#' @examples
#' data("mtcars")
#' SR_correlation_plot(df = mtcars)
#'
#' @export
SR_correlation_plot <- function(df,
                                cor_threshold = NULL,
                                save = FALSE, filename = "NA_Correlation.png",
                                path_output = NULL) {
  # select numeric, integer and date variables
  df2 <- df %>%
    purrr::keep(SR_is_date) %>%
    sapply(as.numeric) %>%
    data.frame()
  df <- df %>%
    purrr::keep(is.numeric)   # includes integer
  if (ncol(df2) > 0) df <- cbind(df, df2)
  rm(df2)
  #
  # remove variables without variation
  df <- SR_remove_column_with_unique_value(df, remove_na = TRUE, silent = TRUE)
  #
  # calculate correlation
  cor_matrix <- stats::cor(df, use = "pairwise.complete.obs")
  cor_matrix[is.na(cor_matrix)] <- 0
  #
  # remove variables with maximum correlations < cor_threshold
  if (!is.null(cor_threshold)) {
    cor_matrix_temp <- data.frame(cor_matrix)
    for (i in 1:ncol(cor_matrix_temp)) {
      cor_matrix_temp[i, i] <- NA
    }; rm(i)
    cor_max <- cor_matrix_temp %>%
      summarise_all(~max(abs(.), na.rm = TRUE)) %>%
      t() %>%
      data.frame() %>%
      setNames("cor_max") %>%
      rownames_to_column() %>%
      filter(cor_max >= cor_threshold)
    cor_matrix <- cor_matrix %>%
      data.frame() %>%
      select(cor_max$rowname) %>%
      filter(colnames(cor_matrix) %in% cor_max$rowname) %>%
      # filter(rownames(.) %in% cor_max$rowname) %>%
      as.matrix(.)
    rownames(cor_matrix) <- cor_max$rowname
  }
  #
  # corrplot
  if (save) {
    grDevices::png(filename = paste0(path_output, filename),
                   height = 1000, width = 1000)
    if (sum(is.na(cor_matrix)) > 0) {
      corrplot::corrplot(cor_matrix, method = "number",
                         type = "lower", diag = FALSE, na.label = "-")
    } else {
      corrplot::corrplot(cor_matrix, method = "number", order = "hclust",
                         hclust.method = "ward.D2",
                         type = "lower", diag = FALSE, na.label = "-")
    }
    try(grDevices::dev.off(), TRUE)
  } else {
    if (sum(is.na(cor_matrix)) > 0) {
      corrplot::corrplot(cor_matrix, method = "number",
                         type = "lower", diag = FALSE, na.label = "-")
    } else {
      corrplot::corrplot(cor_matrix, method = "number", order = "hclust",
                         hclust.method = "ward.D2",
                         type = "lower", diag = FALSE, na.label = "-")
    }
    # corrplot.mixed(cor_matrix)
    assign("cor_matrix", cor_matrix, envir = .GlobalEnv)
  }
}
