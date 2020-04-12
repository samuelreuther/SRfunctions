#' Emblem-Plots one-way
#'
#' Generates different univariate plots for all variables in a data.frame vs. y
#' source: https://www.kaggle.com/peatle/eda-investigating-trends/notebook
#'
#' @param df data.frame
#' @param line character
#' @param bars character
#' @param factor character
#'
#' @return ggplot
#'
#' @examples
#' data("mtcars")
#' SR_plot_one_way(mtcars %>%
#'                   dplyr::select(hp, cyl, mpg) %>%
#'                   dplyr::mutate(cyl = as.factor(cyl)),
#'                 line = "mpg",
#'                 factor = "cyl)
#'
#' @export
# SR_plot_one_way <- function(df, line, bars = NULL, factor){
#   ## If bars not given, do count
#   if (is.null(bars)) {
#     bars <- "Count"
#     df$Count <- 1
#   }
#   #
#   ## Check df contains line, bars, factor
#   if (prod(c(line, bars, factor) %in% names(df)) == 0) {
#     stop("line, bars and factor not all present in df")
#   }
#   #
#   if (!is.factor(df[, factor])) {
#     df[,factor] <- as.factor(df[,factor])
#   }
#   #
#   ## Weight line by bars for weighted average
#   df[,line] <- df[,line] * df[,bars]
#   #
#   ## Crunch line (1 row per factor level and line)
#   df.melt <- reshape2::melt(df[, c(factor, line)], id = factor)
#   df.crunch <- as.data.frame(data.table::data.table(df.melt)[, .(value = sum(value)),
#                                                              by = c(factor, "variable")])
#   #
#   ## Crunch weight (1 row per factor level)
#   df$wt <- df[,bars]
#   df.crunch.wt <- as.data.frame(data.table::data.table(df)[, .(wt = sum(wt)), by = c(factor)])
#   #
#   ## Join weight to line data
#   df.crunch$wt <- df.crunch.wt[match(df.crunch[, factor], df.crunch.wt[, factor]), "wt"]
#   #
#   ## Average response
#   line.avg <- sum(df.crunch$value)/sum(df.crunch$wt)
#   #
#   ## Convert value to average
#   df.crunch$value <- df.crunch$value/df.crunch$wt
#   #
#   ## Rescale weight so that max == line.avg
#   df.crunch.wt$wt_rescaled <- df.crunch.wt$wt * line.avg / max(df.crunch.wt$wt)
#   #
#   ## Plot a chart
#   plot.one_way <- ggplot2::ggplot(df.crunch.wt,
#                                   ggplot2::aes_string(x = factor, y = "wt_rescaled", group = 1)) +
#     ggplot2::geom_bar(stat = "identity", fill = "yellow", colour = "white", alpha = 0.3) +
#     ggplot2::geom_line(data = df.crunch,
#                        ggplot2::aes_string(x = factor, y = "value",
#                                            colour = "variable", group = "variable")) +
#     ggplot2::geom_point(data = df.crunch,
#                         ggplot2::aes_string(x = factor, y = "value",
#                                             colour = "variable",group = "variable")) +
#     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
#     ggplot2::labs(y = "value" )
#   #
#   return(plot.one_way)
# }
