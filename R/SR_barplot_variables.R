SR_barplot_variables <- function(df, path_output = "", save = F) {
  for (i in 1:ncol(df)) {
    #
    # x
    x <- colnames(df)[i]
    print(paste0(i, " / ", ncol(df), ": ", x))
    #
    # plot
    if (class(df[, i]) %in% c("factor", "character") | length(unique(df[, i])) < 30) {
      a <- ggplot(df, aes(x = df[, x])) + geom_bar() + labs(x = x, y = "Count")
    } else {
      a <- ggplot(df, aes(x = df[, x])) + geom_histogram(bins = 30) + labs(x = x, y = "Count")
    }
    print(a)
    #
    # save graphic
    if (save) ggsave(paste0(path_output, x, ".png"), plot = a, width = 9.92, height = 5.3)
  }
  #
  # clean up
  rm(a, x)
}
