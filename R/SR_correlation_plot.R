#' Correlation plot
#'
#' Plot correlation of all numeric, integer and date variables of a data.frame.
#'
#' @param df data.frame
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
