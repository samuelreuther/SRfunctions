# source: https://stackoverflow.com/questions/19233365/how-to-create-a-marimekko-mosaic-plot-in-ggplot2
#
SR_mosaicplot <- function(var1, var2, name_x = NULL, i = NULL, significance = NULL){
  levVar1 <- length(levels(var1))
  # levVar2 <- length(levels(var2))
  #
  jointTable <- prop.table(table(var1, var2))
  plotData <- as.data.frame(jointTable)
  plotData$marginVar1 <- prop.table(table(var1))
  plotData$var2Height <- plotData$Freq / plotData$marginVar1
  plotData$var1Center <- c(0, cumsum(plotData$marginVar1)[1:levVar1 - 1]) +
    plotData$marginVar1 / 2
  #
  p <- ggplot(plotData, aes(var1Center, var2Height)) +
    # geom_bar(stat = "identity", aes(fill = var2), col = "Black") +
    geom_bar(stat = "identity", aes(width = marginVar1, fill = var2), col = "Black") +
    geom_text(aes(label = as.character(var1), x = var1Center, y = 1.05)) +
    scale_x_continuous(labels = percent) + scale_y_continuous(labels = percent)
  if (!is.null(i) & !is.null(significance)) {
    p <- p +
      labs(x = paste0("Anteil: ", name_x), y = "Anteil: y", fill = "y",
           title = paste0(i, ".) ", name_x, " (p-Wert = ", significance, ")"))
  } else {
    p <- p +
      labs(title = "Confusion Matrix", y = "Predicted y", x = "Actual y", fill = "y")
  }
  return(p)
}
