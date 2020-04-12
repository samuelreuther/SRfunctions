#' Emblem-Plots two-way
#'
#' Generates different bivariate plots for all variables in a data.frame vs. y
#' source: https://www.kaggle.com/peatle/eda-investigating-trends/notebook
#'
#' @param df data.frame
#' @param line character
#' @param bars character
#' @param factor1 character
#' @param factor2 character
#' @param rescale boolean
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
# SR_plot_two_way <- function(df, line, bars = NULL, factor1, factor2, rescale = FALSE){
#   ## If bars not given, do count
#   if (is.null(bars)) {
#     bars <- "Count"
#     df$Count <- 1
#   }
#   #
#   ## Check df contains line, bars, factor
#   if (prod(c(line, bars, factor1, factor2) %in% names(df)) == 0) {
#     stop("line, bars and factor not all present in df")
#   }
#   #
#   if (!is.factor(df[, factor1])) {
#     df[, factor1] <- as.factor(df[, factor1])
#   }
#   #
#   if (!is.factor(df[,factor2])) {
#     df[, factor2] <- as.factor(df[, factor2])
#   }
#   #
#   ## Weight line by bars for weighted average
#   df[, line] <- df[, line] * df[, bars]
#   #
#   ## Crunch data
#   df$wt <- df[, bars]
#   df$value <- df[, line]
#   df.crunch <- as.data.frame(data.table::data.table(df)[, .(value = sum(value), wt = sum(wt)),
#                                                         by = c(factor1, factor2)])
#   #
#   ## Crunch weight 1-way
#   df.crunch.1way <- as.data.frame(data.table::data.table(df)[, .(wt = sum(wt)), by = c(factor1)])
#   #
#   ## Convert value to average
#   df.crunch$value <- df.crunch$value/df.crunch$wt
#   #
#   ## Rescale
#   if (rescale) {
#     df.crunch$lp <- ifelse(df.crunch$value %in% c(0, 1),
#                            0, log(df.crunch$value / (1 - df.crunch$value)))
#     lm.noint <- stats::lm(stats::as.formula(paste("lp~", factor1, "+", factor2)),
#                           data = df.crunch,weights = df.crunch$wt)
#     df.crunch$lp_noint <- stats::predict(lm.noint,newdata = df.crunch)
#     df.crunch$lp_int <- df.crunch$lp - df.crunch$lp_noint
#     df.crunch$value <- exp(df.crunch$lp_int) / (1 + exp(df.crunch$lp_int))
#   }
#   #
#   ## Average response
#   line.avg <- sum(df.crunch$value*df.crunch$wt)/sum(df.crunch$wt)
#   #
#   ## Rescale weight so that max == line.avg
#   df.crunch$wt_rescaled <- df.crunch$wt * line.avg / max(df.crunch.1way$wt)
#   #
#   df.crunch <- df.crunch[order(df.crunch[, factor1], df.crunch[, factor2]), ]
#   #
#   ## Plot a chart
#   plot.two_way <- ggplot2::ggplot(df.crunch) +
#     ggplot2::geom_bar(stat = "identity",
#                       ggplot2::aes_string(x = factor1, y = "wt_rescaled", fill = factor2,
#                                           group = factor2), col = "white", alpha = 0.3) +
#     ggplot2::geom_line(ggplot2::aes_string(x = factor1, y = "value",
#                                            colour = factor2, group = factor2)) +
#     ggplot2::geom_point(ggplot2::aes_string(x = factor1, y = "value",
#                                             colour = factor2, group = factor2)) +
#     ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 60, hjust = 1)) +
#     ggplot2::labs(y = "value" )
#   #
#   return(plot.two_way)
# }
