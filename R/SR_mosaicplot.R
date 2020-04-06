#' Mosaic plot
#'
#' Plot a frequency count for example a confusion matrix (true y vs. predicted y).
#'
#' @param var1 factor variable
#' @param var2 factor variable
#'
#' @return ggplot-graphic
#'
#' @example
#' SR_mosaicplot(var1 = c(0, 0, 0, 1, 1, 1),
#'               var2 = c(1, 0, 0, 0, 1, 1))
#'
#' @export
SR_mosaicplot <- function(var1, var2) {
  # load some libraries
  suppressMessages(library(dplyr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(scales))
  #
  # too many levels for plot
  if (length(unique(var1)) > 20 | length(unique(var1)) > 20) {
    cat("Error: variables have too many levels!")
    return(invisible(NULL))
  }
  #
  # generate plot
  # suppressing warning: Ignoring unknown aesthetics: width
  suppressWarnings(
    p <- data.frame(var1, var2) %>%
      mutate_if(is.numeric, as.factor) %>%
      group_by(var1, var2) %>%
      summarise(n = n()) %>%
      mutate(n_percent = n / sum(n),
             x.width = sum(n)) %>%
      ggplot(aes(x = var1, y = n)) +
      geom_col(aes(width = x.width, fill = var2),
               colour = "black", size = 0.5, position = position_fill(reverse = TRUE)) +
      geom_label(aes(label = n),
                 position = position_fill(vjust = 0.5)) +
      facet_grid(.~var1, space = "free", scales = "free", switch = "x") +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Var1", y = "Var2", fill = "Var2",
           title = "Mosaic plot") +
      theme(axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.title.y = element_blank(),
            strip.background = element_blank(),
            panel.spacing = unit(0, "pt")))
  #
  # return plot
  return(p)
}
