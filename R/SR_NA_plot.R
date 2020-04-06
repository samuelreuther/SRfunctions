#' NA plot
#'
#' Plots (1) the occurence of NAs and (2) correlation of them in a data.frame.
#'
#' @param df data.frame
#' @param save Boolean
#' @param filename character, must end with "*png"
#' @param path_output character
#'
#' @return matrix, prints 2 plots
#'
#' @example
#' SR_NA_plot(df = data.frame(numeric = c(0, NA, 0.3, 0.5, NA, 1),
#'                            character = c("a", "b", NA, "d", NA, "f"),
#'                            integer = c(0L, NA, 3L, 5L, NA, 1L)))
#'
#' @export
SR_NA_plot <- function(df,
                       save = FALSE, filename = "NA_Plot.png", path_output = path_output) {
  # load some libraries
  suppressMessages(library(dplyr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(scales))
  #
  # calculate NAs
  missings <- data.frame(VarName = names(df),
                         NAs = sapply(df, function(x) sum(is.na(x))),
                         NAs_in_p = sapply(df, function(x) sum(is.na(x))/nrow(df)))
  missings <- missings[missings$NAs > 0, ]
  missings <- missings[order(-missings[, 2]), ]
  rownames(missings) <- NULL
  #
  # plot NAs
  a <- qplot(NAs_in_p, reorder(VarName, NAs), data = missings, ylab = "Variable") +
    scale_x_continuous(labels = scales::percent, limits = c(0,1)) + xlab("Anteil NAs pro Variable")
  print(a)
  if (save) {
    try(ggsave(paste0(path_output, filename), plot = a, width = 9.92, height = 5.3))
  }
  print(paste0("Proportion NAs in df: ", sum(is.na(df)) / (ncol(df)*nrow(df))))
  print(missings)  # [1:min(which(missings$NAs_in_p == 0)) - 1, ]
  # plot correlation of NAs
  if (nrow(missings) > 1) {
    na_dummy <- df %>% select(one_of(as.character(missings$VarName)))
    # na_dummy <- data.frame()
    for (i in as.character(missings$VarName)) na_dummy[, i] <- ifelse(is.na(df[, i]), 1, 0)
    SR_correlation_plot(na_dummy, save = save,
                        filename = paste0(gsub(".png", "", filename), "_Correlation.png"),
                        path_output = path_output)
  }
  # p_load(VIM)
  # aggr_plot <- aggr(df[sample(10000),], col = c('navyblue','red'), numbers = TRUE,
  #                   sortVars = TRUE, labels = names(data), cex.axis = .7, gap = 3,
  #                   ylab = c("Frequency of missings","Pattern"))
  # marginplot(df[sample(10000), c(1,2)]) # Check if MCAR (missing completely at random)
  #
}
