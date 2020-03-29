SR_y_vs_ydach <- function(obs, pr, save = F, file_name = NULL){
  p_load(Metrics)
  df <- data.frame(y = obs[], pr = pr[])
  p <- ggplot(df, aes(x = y, y = pr)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1, colour = "red", linetype = "dashed") +
    labs(y = "model prediction", title = "y vs. model prediction",
         subtitle = paste0("RMSE:    ", round(rmse(df$y, df$pr), 3),
                           "\nMAE:      ", round(Metrics::mae(df$y, df$pr), 3),
                           # "\nRMSPE: ", round(sqrt(mean((df$pr/df$y - 1)^2)), 3),
                           "\nMAPE:    ", round(sum(abs(df$pr / df$y - 1)) / length(df$y), 3),
                           "\nR2:          ", format(cor(df$y, df$pr)^2, digits = 3))) # ,
  # "\nAUC:          ", round(pROC::auc(df$y, df$pr), 3)))
  print(p)
  if (save) {
    if (is.null(file_name)) file_name <- "y_vs_ydach.png"
    ggsave(paste0(file_name), width = 9.92, height = 5.3)  # 4.67
  }
}
