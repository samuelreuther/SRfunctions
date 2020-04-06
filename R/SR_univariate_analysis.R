#' Calculate univarate analysis
#'
#' Generates different univariate plots for all variables in a data.frame vs. y
#'
#' @param df data.frame
#' @param y_name character
#' @param path_output character
#' @param save Boolean
#'
#' @return Boolean value TRUE or FALSE
#'
#' @example
#' data("mtcars")
#' SR_univariate_analysis(mtcars %>%
#'                          select(hp, cyl, mpg) %>%
#'                          mutate(cyl = as.factor(cyl)),
#'                        y_name = "mpg", save = FALSE)
#'
#' @export
SR_univariate_analysis <- function(df, y_name,
                                   path_output = path_output, save = TRUE) {
  # load some libraries
  suppressMessages(library(dplyr))
  suppressMessages(library(ggplot2))
  suppressMessages(library(scales))
  #
  # Check if y is in df
  if (!y_name %in% colnames(df)) {
    print("Error: Can't find y in df !!!")
    stop()
  }
  #
  # Check and maybe create output folder
  if (save) ifelse(!dir.exists(paste0(path_output,"/Univariate")),
                   dir.create(paste0(path_output,"/Univariate")), FALSE)
  #
  # Sample if df has more than 10000 rows
  if (nrow(df) > 10000) {
    set.seed(1)
    y_temp <- df %>% select(one_of(y_name)) %>% sample_n(10000, replace = FALSE) %>% data.frame()
    set.seed(1)
    df <- df %>% select(-one_of(y_name)) %>% sample_n(10000, replace = FALSE) %>% data.frame()
  } else {
    y_temp <- df %>% select(one_of(y_name)) %>% data.frame()
    df <- df %>% select(-one_of(y_name)) %>% data.frame()
  }
  #
  # convert character as.factor
  df <- df %>%
    mutate_if(is.character, as.factor)
  #
  # Prepare table for summary of all significance tests
  significance <- data.frame(No = 1:ncol(df), Var = NA, P_Value = NA)
  rounding <- 3
  #
  # Plot and test for all variables
  for (i in c(1:ncol(df))) {
    #
    # print progress of loop
    print(paste0(i, " von " , ncol(df), ": ", names(df)[i]))
    #
    # if y is binary, format as factor
    # if (length(unique(y_temp))==2) y_temp <- as.factor(y_temp)
    #
    # prepare data
    temp <- data.frame(y = y_temp[, 1], x = df[, i])
    temp <- SR_feat_eng_factors(temp, make_na_explicit = TRUE)
    temp <- na.omit(temp)
    # check if x = as.numeric(date)
    if (sum(is.na(as.Date(as.character(temp$x), format = "%Y%m%d"))) == 0) {
      temp$x <- as.Date(as.character(temp$x), format = "%Y%m%d")
    }
    # check if x is LabelEnc
    if (exists("factor_encoding")) {
      if (gsub("_LabelEnc", "", names(df)[i]) %in% factor_encoding$feature) {
        temp$x <- factor(temp$x)
        levels(temp$x) <- factor_encoding$levels[factor_encoding$feature ==
                                                   gsub("_LabelEnc", "", names(df)[i])]
      }
    }
    #
    # skip variable if levels(x)==1
    if (length(unique(temp$x)) == 1) next
    #
    # calculate frequency of each value (for size of geom_point)
    if (length(unique(temp$x)) < 30) {
      temp <- merge(temp,
                    as.data.frame(table(temp[, 1], temp[, 2])/nrow(temp)),
                    by.x = c("y","x"), by.y = c("Var1", "Var2"), all.x = TRUE, sort = FALSE)
    }
    significance$Var[i] <- paste0(i, ".) ", names(df)[i])
    #
    # Generate grafics
    if (is.factor(temp$y)) {
      if (is.factor(temp$x)) {
        # y: factor   x: factor
        # calculate significance ANOVA
        significance$P_Value[i] <-
          round(chisq.test(temp$x, y = temp$y, p = rep(1/length(temp), length(temp$x)),
                           simulate.p.value = TRUE, B = 100)[[3]], rounding)
        # noch ändern!!!
        p <- SR_mosaicplot(var1 = temp$x, var2 = temp$y)
        # p <- SR_mosaicplot(var1 = temp$x, var2 = temp$y, name_x = names(df)[i])
        # qplot(data = temp, y = y, x = x, colour = y, geom = "jitter",
        #       main = paste0(i, ".) ", colnames(temp)[i], " (p-Wert=",
        #                   format(significance$P_Value[i], nsmall = 3), ")"),
        #       xlab = colnames(df)[i])
        # ggplot(temp, aes(x = x, y = y)) + geom_smooth()
        # print(SR_mosaicplot(x~y, data = temp, color = TRUE))
      } else {
        # y: factor   x: numeric
        # calculate significance ANOVA
        significance$P_Value[i] <-
          round(summary(aov(temp$x ~ temp$y))[[1]][["Pr(>F)"]][[1]], rounding)
        p <- qplot(data = temp, x = x, geom = "density", color = y,
                   main = paste0(i, ".) ", names(df)[i], " (p-Wert=",
                                 format(significance$P_Value[i], nsmall = 3),")"),
                   xlab = names(df)[i]) +
          scale_x_continuous(breaks = pretty_breaks(6))
      }
    } else {
      if (is.factor(temp$x)) {
        # y: numeric   x: factor
        # calculate significance ANOVA
        significance$P_Value[i] <-
          round(summary(aov(temp$y ~ temp$x))[[1]][["Pr(>F)"]][[1]], rounding)
        p <- qplot(data = temp, y = y, x = x, geom = "jitter",
                   main = paste0(i, ".) ", names(df)[i], " (p-Wert=",
                                 format(significance$P_Value[i], nsmall = 3),")"),
                   xlab = names(df)[i]) +
          stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax = "mean",
                       size = 0.3, geom = "crossbar", colour = "blue") +
          scale_y_continuous(breaks = pretty_breaks(6))
        } else {
        # y: numeric   x: numeric
        # calculate significance ANOVA
        significance$P_Value[i] <-
          round(summary(aov(temp$y ~ temp$x))[[1]][["Pr(>F)"]][[1]], rounding)
        # calculate bins for x
        binned <- temp %>%
          mutate(Group = cut(x, breaks = pretty(x, 40), include.lowest = TRUE)) %>%
          group_by(Group) %>%
          summarise(y = mean(y),
                    x = median(x),
                    Count = n())
        if (length(unique(temp$x)) < 30) {
          p <- qplot(data = temp, y = y, x = x, size = Freq,
                     main = paste0(i, ".) ", names(df)[i], " (p-Wert=",
                                   format(significance$P_Value[i], nsmall = 3),")"),
                     xlab = names(df)[i]) +
            geom_smooth(method = "loess") +
            scale_x_continuous(breaks = pretty_breaks(6)) +
            scale_y_continuous(breaks = pretty_breaks(6))
        } else {
          p <- qplot(data = temp, y = y, x = x,
                     main = paste0(i, ".) ", names(df)[i], " (p-Wert=",
                                   format(significance$P_Value[i], nsmall = 3),")"),
                     xlab = names(df)[i]) +
            geom_smooth(method = "loess") +
            scale_x_continuous(breaks = pretty_breaks(6)) +
            scale_y_continuous(breaks = pretty_breaks(6))
        }
      }
    }
    print(p)
    if (save) {
      graphic_name <- paste0(path_output, "Univariate/", i, ". ", y_name, " ~ ",
                             names(df)[i], ".png")
      ggsave(graphic_name, plot = p, width = 9.92, height = 5.3)  # 4.67
    }
  }  # next i
  #
  # Significance: save plot and table
  significance$No <- NULL
  significance$Var <- as.factor(significance$Var)
  significance <- significance[order(significance$P_Value), ]
  significance <- na.omit(significance)
  significance <- droplevels(significance)
  q <- qplot(data = significance, y = reorder(Var, -P_Value), x = P_Value,
             ylab = "Variable", xlim = c(0, 1))
  print(q)
  if (save) {
    ggsave(paste0(path_output, "/Univariate/0. All Variables.png"),
           plot = q, width = 9.92, height = 5.3)  # 4.67
    write.table(significance,
                file = paste0(path_output, "/Univariate/0. All Variables.csv"),
                row.names = FALSE, col.names = TRUE, append = FALSE, sep = ";", dec = ",")
  }
  #
  return(invisible(NULL))
}
