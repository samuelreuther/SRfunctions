#' Plot true y vs. predicted y
#'
#' e.g. plot a model prediction for y against true y values and calculates several precision metrics.
#'
#' @param true_y numeric
#' @param predicted_y numeric
#' @param path_output character
#' @param save Boolean
#'
#' @return Boolean value TRUE or FALSE
#'
#' @examples
#' data("mtcars")
#' SR_plot_true_y_VS_predicted_y(true_y = mtcars$hp,
#'                               predicted_y = mtcars$hp +
#'                                 rnorm(nrow(mtcars), sd = 30))
#'
#' @export
SR_plot_true_y_VS_predicted_y <- function(true_y, predicted_y,
                                          path_output = NULL, save = FALSE){
  df <- data.frame(y = true_y[], pr = predicted_y[])
  p <- ggplot2::ggplot(df, ggplot2::aes(x = y, y = pr)) +
    ggplot2::geom_point() +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
    ggplot2::labs(y = "model prediction", title = "y vs. model prediction",
         subtitle = paste0("RMSE:    ", round(Metrics::rmse(df$y, df$pr), 3),
                           "\nMAE:      ", round(Metrics::mae(df$y, df$pr), 3),
                           "\nMAPE:    ", round(sum(abs(df$pr / df$y - 1)) / length(df$y), 3),
                           "\nR2:          ", format(stats::cor(df$y, df$pr)^2, digits = 3))) # ,
  # "\nAUC:          ", round(pROC::auc(df$y, df$pr), 3)))
  print(p)
  if (save) ggplot2::ggsave(paste0(path_output, "y_vs_ydach.png"),
                            width = 9.92, height = 5.3)  # 4.67
  #
  return(invisible(NULL))
}
