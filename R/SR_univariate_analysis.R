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
#' @examples
#' data("mtcars")
#' SR_univariate_analysis(mtcars %>%
#'                          dplyr::select(hp, cyl, mpg) %>%
#'                          dplyr::mutate(cyl = as.factor(cyl)),
#'                        y_name = "mpg", save = FALSE)
#'
#' @export
SR_univariate_analysis <- function(df, y_name,
                                   path_output = path_output, save = TRUE) {
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
    y_temp <- df %>%
      dplyr::select(dplyr::one_of(y_name)) %>%
      dplyr::sample_n(10000, replace = FALSE) %>%
      data.frame()
    set.seed(1)
    df <- df %>%
      dplyr::select(-dplyr::one_of(y_name)) %>%
      dplyr::sample_n(10000, replace = FALSE) %>%
      data.frame()
  } else {
    y_temp <- df %>%
      dplyr::select(dplyr::one_of(y_name)) %>%
      data.frame()
    df <- df %>%
      dplyr::select(-dplyr::one_of(y_name)) %>%
      data.frame()
  }
  #
  # convert character as.factor
  df <- df %>%
    dplyr::mutate_if(is.character, as.factor)
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
    temp <- SR_feat_eng_factors(temp, make_na_explicit = TRUE, label_encoding = FALSE)
    temp <- stats::na.omit(temp)
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
          round(stats::chisq.test(temp$x, y = temp$y,
                                  p = rep(1/length(temp), length(temp$x)),
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
          round(summary(stats::aov(temp$x ~ temp$y))[[1]][["Pr(>F)"]][[1]], rounding)
        p <- ggplot2::ggplot(data = temp,
                             ggplot2::aes(x = x, geom = "density", color = y)) +
          ggplot2::geom_density() +
          ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
          ggplot2::labs(title = paste0(i, ".) ", names(df)[i], " (p-Wert=",
                                       format(significance$P_Value[i], nsmall = 3),")"),
                        x = names(df)[i])
        # p <- ggplot2::qplot(data = temp, x = x, geom = "density", color = y,
        #                     main = paste0(i, ".) ", names(df)[i], " (p-Wert=",
        #                                   format(significance$P_Value[i], nsmall = 3),")"),
        #                     xlab = names(df)[i]) +
        #   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6))
      }
    } else {
      if (is.factor(temp$x)) {
        # y: numeric   x: factor
        # calculate significance ANOVA
        significance$P_Value[i] <-
          round(summary(stats::aov(temp$y ~ temp$x))[[1]][["Pr(>F)"]][[1]], rounding)
        p <- ggplot2::ggplot(data = temp, ggplot2::aes(x = x, y = y)) +
          ggplot2::geom_jitter() +
          ggplot2::stat_summary(fun = "mean", fun.min = "mean", fun.max = "mean",
                                linewidth = 0.3, geom = "crossbar", colour = "blue") +
          ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
          ggplot2::labs(title = paste0(i, ".) ", names(df)[i], " (p-Wert=",
                                       format(significance$P_Value[i], nsmall = 3),")"),
                        x = names(df)[i])
        # p <- ggplot2::qplot(data = temp, y = y, x = x, geom = "jitter",
        #                     main = paste0(i, ".) ", names(df)[i], " (p-Wert=",
        #                                   format(significance$P_Value[i], nsmall = 3),")"),
        #                     xlab = names(df)[i]) +
        #   ggplot2::stat_summary(fun = "mean", fun.min = "mean", fun.max = "mean",
        #                         linewidth = 0.3, geom = "crossbar", colour = "blue") +
        #   ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6))
      } else {
        # y: numeric   x: numeric
        # calculate significance ANOVA
        significance$P_Value[i] <-
          round(summary(stats::aov(temp$y ~ temp$x))[[1]][["Pr(>F)"]][[1]], rounding)
        # calculate bins for x
        binned <- temp %>%
          dplyr::mutate(Group = cut(x, breaks = pretty(x, 40), include.lowest = TRUE)) %>%
          dplyr::group_by(Group) %>%
          dplyr::summarise(y = mean(y),
                           x = stats::median(x),
                           Count = dplyr::n())
        if (length(unique(temp$x)) < 30) {
          p <- ggplot2::ggplot(data = temp, ggplot2::aes(x = x, y = y, size = Freq)) +
            ggplot2::geom_point() +
            ggplot2::geom_smooth(method = "loess", formula = "y ~ x") +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
            ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
            ggplot2::labs(title = paste0(i, ".) ", names(df)[i], " (p-Wert=",
                                         format(significance$P_Value[i], nsmall = 3),")"),
                          x = names(df)[i])
          # p <- ggplot2::qplot(data = temp, y = y, x = x, size = Freq,
          #                     main = paste0(i, ".) ", names(df)[i], " (p-Wert=",
          #                                   format(significance$P_Value[i], nsmall = 3),")"),
          #                     xlab = names(df)[i]) +
          #   ggplot2::geom_smooth(method = "loess", formula = "y ~ x") +
          #   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
          #   ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6))
        } else {
          p <- ggplot2::ggplot(data = temp, ggplot2::aes(x = x, y = y)) +
            ggplot2::geom_point() +
            ggplot2::geom_smooth(method = "loess", formula = "y ~ x") +
            ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
            ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6)) +
            ggplot2::labs(title = paste0(i, ".) ", names(df)[i], " (p-Wert=",
                                         format(significance$P_Value[i], nsmall = 3),")"),
                          x = names(df)[i])
          # p <- ggplot2::qplot(data = temp, y = y, x = x,
          #                     main = paste0(i, ".) ", names(df)[i], " (p-Wert=",
          #                                   format(significance$P_Value[i], nsmall = 3),")"),
          #                     xlab = names(df)[i]) +
          #   ggplot2::geom_smooth(method = "loess", formula = "y ~ x") +
          #   ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(6)) +
          #   ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6))
        }
      }
    }
    suppressWarnings({   # strange warning since ggplot2 3.4.0
      print(p)
    })
    if (save) {
      graphic_name <- paste0(path_output, "Univariate/", i, ". ", y_name, " ~ ",
                             names(df)[i], ".png")
      ggplot2::ggsave(graphic_name, plot = p, width = 9.92, height = 5.3)  # 4.67
    }
  }  # next i
  #
  # Significance: save plot and table
  significance$No <- NULL
  significance$Var <- as.factor(significance$Var)
  significance <- significance[order(significance$P_Value), ]
  significance <- stats::na.omit(significance)
  significance <- droplevels(significance)
  q <- ggplot2::ggplot(data = significance,
                       ggplot2::aes(x = P_Value, y = stats::reorder(Var, -P_Value))) +
    ggplot2::geom_point() +
    ggplot2::scale_x_continuous(limits = c(0, 1)) +
    ggplot2::labs(y = "Variable")
  # q <- ggplot2::qplot(data = significance,
  #                     y = stats::reorder(Var, -P_Value), x = P_Value,
  #                     ylab = "Variable", xlim = c(0, 1))
  print(q)
  if (save) {
    ggplot2::ggsave(paste0(path_output, "/Univariate/0. All Variables.png"),
                    plot = q, width = 9.92, height = 5.3)  # 4.67
    utils::write.table(significance,
                       file = paste0(path_output, "/Univariate/0. All Variables.csv"),
                       row.names = FALSE, col.names = TRUE, append = FALSE,
                       sep = ";", dec = ",")
  }
  #
  return(invisible(NULL))
}
