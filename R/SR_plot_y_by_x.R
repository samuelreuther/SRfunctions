#' Plot y by x
#'
#' Generates different univariate plots, depending if x/y are numeric, or factor/character
#'
#' @param df data.frame
#' @param x_name character
#' @param y_name character
#' @param path_output character
#' @param save Boolean
#'
#' @return Null, generates plot
#'
#' @examples
#' data("mtcars")
#' SR_plot_y_by_x(mtcars,
#'                x = "hp", y = "mpg")
#' SR_plot_y_by_x(mtcars %>% dplyr::mutate(hp = as.factor(hp)),
#'                x = "hp", y = "mpg")
#'
#' @export
SR_plot_y_by_x <- function(df, x_name, y_name,
                           path_output = path_output, save = FALSE) {
  # prepare data
  temp <- data.frame(y = df[, y_name], x = df[, x_name]) %>%
    dplyr::mutate_if(is.character, as.factor)
  # temp <- stats::na.omit(temp)
  #
  # if (length(unique(temp$x))<30) {
  #   temp <- merge(temp, as.data.frame(table(temp[, 1], temp[, 2])/length(temp)),
  #                 by.x = c("y","x"), by.y = c("Var1", "Var2"), all.x = TRUE, sort = FALSE)
  # }
  #
  # plot
  if (is.factor(temp$y)) {
    if (is.factor(temp$x)) {      # y: factor   x: factor
      p <- ggplot2::ggplot(temp, ggplot2::aes(x = x, y = y)) +
        ggplot2::stat_bin2d(ggplot2::aes(fill = ..count..)) +
        ggplot2::stat_bin2d(geom = "text", ggplot2::aes(label = ..count..)) +
        ggplot2::scale_fill_gradient(low = "white", high = "blue") +
        ggplot2::scale_y_discrete(limits = rev(levels(temp$y))) +
        ggplot2::labs(title = x_name)
      # SR_mosaicplot(var1 = temp$x_name, var2 = temp$y)
    } else {
      # y: factor x: numeric
      p <- ggplot2::ggplot(data = temp, ggplot2::aes(x = x, color = y)) +
        ggplot2::geom_density() +
        ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5)) +
        ggplot2::labs(title = x_name, x = x_name, colour = y_name)
      # p <- ggplot2::qplot(data = temp, x = x, geom = "density", color = y) +
      #   ggplot2::labs(title = x_name, x = x_name, colour = y_name) +
      #   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5))
    }
  } else {
    if (is.factor(temp$x)) {     # y: numeric   x: factor
      p <- ggplot2::ggplot(data = temp, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_jitter() +
        ggplot2::stat_summary(fun = "mean", fun.min = "mean", fun.max = "mean",
                              size = 0.3, geom = "crossbar", colour = "blue") +
        ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5)) +
        ggplot2::labs(title = x_name, x = x_name, y = y_name)
      # p <- ggplot2::qplot(data = temp, y = y, x = x, geom = "jitter") +
      #   ggplot2::labs(title = x_name, x = x_name, y = y_name) +
      #   ggplot2::stat_summary(fun = "mean", fun.min = "mean", fun.max = "mean",
      #                         size = 0.3, geom = "crossbar", colour = "blue") +
      #   ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5))
    } else {
      # y: numeric x: numeric
      if (length(unique(temp$x)) < 100) {
        p <- ggplot2::ggplot(data = temp, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_count() +
          ggplot2::geom_smooth(method = "loess") +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5)) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5)) +
          ggplot2::labs(title = x_name, x = x_name, y = y_name)
        # p <- ggplot2::qplot(data = temp, y = y, x = x) +
        #   ggplot2::geom_count() +
        #   ggplot2::labs(title = x_name, x = x_name, y = y_name) +
        #   ggplot2::geom_smooth(method = "loess") +
        #   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5)) +
        #   ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5))
      } else {
        p <- ggplot2::ggplot(data = temp, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_smooth(method = "loess") +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5)) +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5)) +
          ggplot2::labs(title = x_name, x = x_name, y = y_name)
        # p <- ggplot2::qplot(data = temp, y = y, x = x) +
        #   ggplot2::geom_smooth(method = "loess") +
        #   ggplot2::labs(title = x_name, x = x_name, y = y_name) +
        #   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(5)) +
        #   ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(5))
      }
    }
  }
  #
  # save graphic
  if (save) ggplot2::ggsave(paste0(path_output, x_name, " - ", y_name, ".png"),
                            plot = p, width = 9.92, height = 5.3)
  #
  # return result
  print(p)
  return(invisible(NULL))
}
