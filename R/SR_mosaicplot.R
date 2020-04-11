#' Mosaic plot
#'
#' Plot a frequency count for example a confusion matrix (true y vs. predicted y).
#'
#' @param var1 factor variable
#' @param var2 factor variable
#'
#' @return ggplot-graphic
#'
#' @examples
#' SR_mosaicplot(var1 = c(0, 0, 0, 1, 1, 1),
#'               var2 = c(1, 0, 0, 0, 1, 1))
#'
#' @export
SR_mosaicplot <- function(var1, var2) {
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
      dplyr::mutate_if(is.numeric, as.factor) %>%
      dplyr::group_by(var1, var2) %>%
      dplyr::summarise(n = dplyr::n()) %>%
      dplyr::mutate(n_percent = n / sum(n),
                    x.width = sum(n)) %>%
      ggplot2::ggplot(ggplot2::aes(x = var1, y = n)) +
      ggplot2::geom_col(ggplot2::aes(width = x.width, fill = var2),
                        colour = "black", size = 0.5,
                        position = ggplot2::position_fill(reverse = TRUE)) +
      ggplot2::geom_label(ggplot2::aes(label = n),
                          position = ggplot2::position_fill(vjust = 0.5)) +
      ggplot2::facet_grid(.~var1, space = "free", scales = "free", switch = "x") +
      ggplot2::scale_y_continuous(labels = scales::percent) +
      ggplot2::labs(x = "Var1", y = "Var2", fill = "Var2",
                    title = "Mosaic plot") +
      ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                     axis.ticks.x = ggplot2::element_blank(),
                     axis.title.y = ggplot2::element_blank(),
                     strip.background = ggplot2::element_blank(),
                     panel.spacing = ggplot2::unit(0, "pt")))
  #
  # return plot
  return(p)
}
