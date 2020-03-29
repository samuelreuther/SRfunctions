SR_correlation_plot <- function(df, plot = T, save = F, filename = "Correlation.png") {
  p_load(corrplot, purrr)
  # select numeric, integer and date variables
  df2 <- df %>%
    keep(SR_is_date) %>%
    sapply(as.numeric) %>%
    data.frame()
  df <- df %>%
    keep(is.numeric)   # includes integer
  if (ncol(df2) > 0) df <- cbind(df, df2)
  rm(df2)
  # remove variables without variation
  df <- SR_remove_column_with_unique_value(df, remove_na = T, silent = T)
  # calculate correlation
  cor_matrix <- cor(df, use = "pairwise.complete.obs")
  cor_matrix[is.na(cor_matrix)] <- 0
  # cor_matrix %>% View()
  # corrplot
  if (save) {
    png(filename = paste0(path_output, filename), height = 1000, width = 1000)
    if (sum(is.na(cor_matrix)) > 0) {
      corrplot(cor_matrix, method = "number",
               type = "lower", diag = F, na.label = "-")
    } else {
      corrplot(cor_matrix, method = "number", order = "hclust", hclust.method = "ward.D2",
               type = "lower", diag = F, na.label = "-")
    }
    try(dev.off(), T)
  } else {
    if (sum(is.na(cor_matrix)) > 0) {
      corrplot(cor_matrix, method = "number",
               type = "lower", diag = F, na.label = "-")
    } else {
      corrplot(cor_matrix, method = "number", order = "hclust", hclust.method = "ward.D2",
               type = "lower", diag = F, na.label = "-")
    }
    # corrplot.mixed(cor_matrix)
    assign("cor_matrix", cor_matrix, envir = .GlobalEnv)
  }
}
