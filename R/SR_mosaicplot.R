#' Mosaic plot (true y vs. predicted y)
#'
#' Plot a frequency count for a binary confusion matrix (true y vs. predicted y).
#'
#' @param df data.frame with columns "y_true" and "predicted_y"
#' @param cutoff numeric (default = 0.5): cutoff value for predicted_y to count as "1"
#'
#' @return ggplot-graphic
#'
#' @examples
#' SR_mosaicplot(df = data.frame(y_true = c(0, 0, 0, 1, 1, 1),
#'                               predicted_y = c(0.8, 0.1, 0.4, 0.2, 0.9, 0.8)),
#'               cutoff = 0.5)
#'
#' @export
SR_mosaicplot <- function(df, cutoff = 0.5) {
  # thanks go to:
  # https://stackoverflow.com/questions/50227916/adding-counts-to-ggmosaic-can-this-be-done-simpler?noredirect=1&lq=1
  #
  # load some libraries
  suppressMessages(library(dplyr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(scales))
  #
  # generate plot
  # suppressing warning: Ignoring unknown aesthetics: width
  suppressWarnings(
    p <- df %>%
      mutate(y_true = factor(y_true, levels = c("0", "1")),
             predicted_y = factor(ifelse(predicted_y <= cutoff, 0, 1), levels = c("0", "1"))) %>%
      group_by(y_true, predicted_y) %>%
      summarise(n = n()) %>%
      mutate(n_percent = n / sum(n),
             x.width = sum(n)) %>%
      ggplot(aes(x = y_true, y = n)) +
      geom_col(aes(width = x.width, fill = predicted_y),
               colour = "black", size = 0.5, position = position_fill(reverse = T)) +
      geom_label(aes(label = n),
                 position = position_fill(vjust = 0.5)) +
      facet_grid(.~y_true, space = "free", scales = "free", switch = "x") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "True y", y = "Predicted y", fill = "Predicted y",
           title = "Confusion Matrix") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            strip.background = element_blank(),
            panel.spacing = unit(0, "pt")))
  #
  # return plot
  return(p)
}
