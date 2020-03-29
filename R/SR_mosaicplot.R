SR_mosaicplot <- function(df, cutoff) {
  # https://stackoverflow.com/questions/50227916/adding-counts-to-ggmosaic-can-this-be-done-simpler?noredirect=1&lq=1
  #
  p <- df %>%
    mutate(y = factor(y, levels = c("0", "1")),
           pr = factor(ifelse(pr <= cutoff, 0, 1), levels = c("0", "1"))) %>%
    group_by(y, pr) %>%
    summarise(n = n()) %>%
    mutate(n_percent = n / sum(n),
           x.width = sum(n)) %>%
    ggplot(aes(x = y, y = n)) +
    geom_col(aes(width = x.width, fill = pr),
             colour = "black", size = 0.5, position = position_fill(reverse = T)) +
    geom_label(aes(label = n),
               position = position_fill(vjust = 0.5)) +
    facet_grid(.~y, space = "free", scales = "free", switch = "x") +
    scale_y_continuous(labels = scales::percent) +
    labs(x = "Actual y", y = "Predicted y", fill = "Predicted y",
         title = "Confusion Matrix") +
    theme(axis.text.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.title.y = element_blank(),
          strip.background = element_blank(),
          panel.spacing = unit(0, "pt"))
  return(p)
}

# SR_mosaicplot_old <- function(var1, var2, name_x = NULL, i = NULL, significance = NULL) {
#   # https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
#   levVar1 <- length(levels(var1))
#   # levVar2 <- length(levels(var2))
#   #
#   jointTable <- prop.table(table(var1, var2))
#   plotData <- as.data.frame(jointTable)
#   plotData$marginVar1 <- prop.table(table(var1))
#   plotData$var2Height <- plotData$Freq / plotData$marginVar1
#   plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 - 1]) +
#     plotData$marginVar1 / 2
#   #
#   p <- ggplot(plotData, aes(var1Center, var2Height)) +
#     # geom_bar(stat = "identity", aes(fill = var2), col = "Black") +
#     geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
#     geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) +
#     scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent)
#   if (!is.null(i) & !is.null(significance)) {
#     p <- p +
#       labs(x = paste0("Anteil: ", name_x), y = "Anteil: y", fill = "y",
#            title = paste0(i, ".) ", name_x, " (p-Wert = ", significance, ")"))
#   } else {
#     p <- p +
#       labs(title = "Confusion Matrix", y = "Predicted y", x = "Actual y", fill = "y")
#   }
#   return(p)
# }
