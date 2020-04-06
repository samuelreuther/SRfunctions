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
#' SR_plot_y_by_x(mtcars %>% mutate(hp = as.factor(hp)),
#'                x = "hp", y = "mpg")
#'
#' @export
SR_plot_y_by_x <- function(df, x_name, y_name,
                           path_output = path_output, save = FALSE) {
  # load some libraries
  suppressMessages(library(dplyr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(scales))
  #
  # prepare data
  temp <- data.frame(y = df[, y_name], x = df[, x_name]) %>%
    mutate_if(is.character, as.factor)
  # temp <- na.omit(temp)
  #
  # if (length(unique(temp$x))<30) {
  #   temp <- merge(temp, as.data.frame(table(temp[, 1], temp[, 2])/length(temp)),
  #                 by.x = c("y","x"), by.y = c("Var1", "Var2"), all.x = TRUE, sort = FALSE)
  # }
  #
  # plot
  if (is.factor(temp$y)) {
    if (is.factor(temp$x)) {      # y: factor   x: factor
      p <- ggplot(temp, aes(x = x, y = y)) +
        stat_bin2d(aes(fill = ..count..)) +
        stat_bin2d(geom = "text", aes(label = ..count..)) +
        scale_fill_gradient(low = "white", high = "blue") +
        scale_y_discrete(limits = rev(levels(temp$y))) +
        labs(title = x_name)
      # SR_mosaicplot(var1 = temp$x_name, var2 = temp$y)
    } else {
      # y: factor x: numeric
      p <- qplot(data = temp, x = x, geom = "density", color = y) +
        labs(title = x_name, x = x_name, colour = y_name) +
        scale_x_continuous(breaks = scales::pretty_breaks(5))
    }
  } else {
    if (is.factor(temp$x)) {     # y: numeric   x: factor
      p <- qplot(data = temp, y = y, x = x, geom = "jitter") +
        labs(title = x_name, x = x_name, y = y_name) +
        stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax = "mean",
                     size = 0.3, geom = "crossbar", colour = "blue") +
        scale_y_continuous(breaks = scales::pretty_breaks(5))
    } else {
      # y: numeric x: numeric
      if (length(unique(temp$x)) < 100) {
        p <- qplot(data = temp, y = y, x = x) +
          geom_count() +
          labs(title = x_name, x = x_name, y = y_name) +
          geom_smooth(method = "loess") +
          scale_x_continuous(breaks = scales::pretty_breaks(5)) +
          scale_y_continuous(breaks = scales::pretty_breaks(5))
      } else {
        p <- qplot(data = temp, y = y, x = x) +
          geom_smooth(method = "loess") +
          labs(title = x_name, x = x_name, y = y_name) +
          scale_x_continuous(breaks = scales::pretty_breaks(5)) +
          scale_y_continuous(breaks = scales::pretty_breaks(5))
      }
    }
  }
  #
  # save graphic
  if (save) ggsave(paste0(path_output, x_name, " - ", y_name, ".png"),
                   plot = p, width = 9.92, height = 5.3)
  #
  # return result
  print(p)
  return(invisible(NULL))
}
