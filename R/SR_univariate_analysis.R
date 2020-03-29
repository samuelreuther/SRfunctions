# x <- train[, c(2:5)]; y_name <- "target"; i <- 2
# x <- train[, c(112:114)]; y_name <- "v110"; i <- 2
# x <- temp; y_name <- "y"; i <- 1
# x <- temp; y_name <- "position"; i <- 2
#
# SR_univariate_analysis(x=df, y_name=y_name)
#
SR_univariate_analysis <- function(x, y_name, summary = T, save = T) {
  #
  # Check if y is in x
  if (!y_name %in% colnames(x)) {
    print("Error: Can't find y in x !!!")
    stop()
  }
  #
  # Check and maybe create output folder
  if (save) ifelse(!dir.exists(paste0(path_output,"/Univariate")),
                   dir.create(paste0(path_output,"/Univariate")), F)
  #
  # Sample if x has more than 10000 rows
  if (nrow(x) > 10000) {
    set.seed(1)
    y_temp <- x %>% select(one_of(y_name)) %>% sample_n(10000, replace = F) %>% data.frame()
    # y_temp <- x[sample.int(nrow(x), size = 10000), y_name]
    set.seed(1)
    x <- x %>% select(-one_of(y_name)) %>% sample_n(10000, replace = F) %>% data.frame()
    # x <- x[sample.int(nrow(x), size = 10000), !colnames(x) %in% y_name]
  } else {
    y_temp <- x %>% select(one_of(y_name)) %>% data.frame()
    # y_temp <- x[, y_name]
    x <- x %>% select(-one_of(y_name)) %>% data.frame()
    # x <- x[, !colnames(x) %in% y_name]
  }
  #
  # Prepare table for summary of all significance tests
  significance <- data.frame(No = 1:ncol(x), Var = NA, P_Value = NA)
  rounding <- 5
  #
  # Plot and test for all variables
  for (i in c(1:ncol(x))) {
    #
    # print progress of loop
    print(paste0(i, " von " , ncol(x), ": ", names(x)[i]))
    #
    # if y is binary, format as factor
    # if (length(unique(y_temp))==2) y_temp <- as.factor(y_temp)
    #
    # prepare data
    temp <- data.frame(y = y_temp[, 1], x = x[, i])
    temp <- SR_feat_eng_factors(temp, make_na_explicit = T)
    temp <- na.omit(temp)
    # check if x = as.numeric(date)
    if (sum(is.na(as.Date(as.character(temp$x), format = "%Y%m%d"))) == 0) {
      temp$x <- as.Date(as.character(temp$x), format = "%Y%m%d")
    }
    # check if x is LabelEnc
    if (exists("factor_encoding")) {
      if (gsub("_LabelEnc", "", names(x)[i]) %in% factor_encoding$feature) {
        temp$x <- factor(temp$x)
        levels(temp$x) <- factor_encoding$levels[factor_encoding$feature ==
                                                   gsub("_LabelEnc", "", names(x)[i])]
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
                    by.x = c("y","x"), by.y = c("Var1", "Var2"), all.x = T, sort = F)
    }
    significance$Var[i] <- paste0(i, ".) ", names(x)[i]) # colnames(x)[i]
    #
    # Generate grafics
    # try(dev.off(), T)
    if (is.factor(temp$y)) {
      if (is.factor(temp$x)) {      # y: factor   x: factor
        significance$P_Value[i] <-
          round(chisq.test(temp$x, y = temp$y, p = rep(1/length(temp), length(temp$x)),
                           simulate.p.value = T, B = 100)[[3]], rounding)
        # if (!summary) {
        # noch ändern!!!
        p <- SR_mosaicplot(var1 = temp$x, var2 = temp$y, name_x = names(x)[i]) # colnames(x)[i]
        # qplot(data = temp, y = y, x = x, colour = y, geom = "jitter",
        #       main = paste0(i, ".) ", colnames(temp)[i], " (p-Wert=",
        #                   significance$P_Value[i], ")"), xlab = colnames(x)[i])
        # ggplot(temp, aes(x = x, y = y)) + geom_smooth()
        # print(SR_mosaicplot(x~y, data = temp, color = T))
        # }
      } else {
        # y: factor   x: numeric
        significance$P_Value[i] <-
          round(summary(aov(temp$x ~ temp$y))[[1]][["Pr(>F)"]][[1]], rounding)
        if (!summary) {
          p <- qplot(data = temp, x = x, geom = "density", color = y,
                     main = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(x)[i]
                                   significance$P_Value[i],")"), xlab = names(x)[i]) + # colnames(x)[i]
            scale_x_continuous(breaks = pretty_breaks(6))
          # qplot(x = y, y = x, geom = "boxplot",
          #       main = paste0(colnames(temp)[i], " (p-Wert=",
          #                   round(summary(aov(temp$x ~ temp$y))[[1]][["Pr(>F)"]][[1]],
          #                         digits = 3),")")) + coord_flip()
        } else {
          # noch ändern!!!
          p <- ggplot(temp, aes(x = x, color = y)) +
            geom_density() +
            labs(title = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(x)[i]
                                significance$P_Value[i],")"), x = names(x)[i]) + # colnames(x)[i]
            scale_x_continuous(breaks = pretty_breaks(6))
        }
      }
    } else {
      if (is.factor(temp$x)) {
        # y: numeric   x: factor
        significance$P_Value[i] <-
          round(summary(aov(temp$y ~ temp$x))[[1]][["Pr(>F)"]][[1]], rounding)
        if (!summary) {
          p <- qplot(data = temp, y = y, x = x, geom = "jitter",
                     main = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(temp)[i]
                                   significance$P_Value[i],")"), xlab = names(x)[i]) + # colnames(x)[i])
            stat_summary(fun.y = "mean", fun.ymin = "mean", fun.ymax = "mean",
                         size = 0.3, geom = "crossbar", colour = "blue") +
            scale_y_continuous(breaks = pretty_breaks(6))
          # qplot(data = temp, y = y, x = x, geom = "boxplot",
          #       main = paste0(i, ".) ", colnames(temp)[i], " (p-Wert=",
          #                   significance$P_Value[i],")"), xlab = colnames(x)[i])
        } else {
          p1 <- ggplot(temp, aes(y = y, x = x)) +
            labs(title = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(temp)[i]
                                significance$P_Value[i],")"), x = names(x)[i]) + # colnames(x)[i])
            geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
            scale_y_continuous(breaks = pretty_breaks(6))
          p2 <- ggplot(temp, aes(x = x)) +
            geom_bar(position = "dodge") +
            scale_y_continuous(breaks = pretty_breaks(4)) +
            labs(title = "", x = names(x)[i])
          p <- arrangeGrob(p1, p2, ncol = 1, heights = c(0.75, 0.25))   # grid.arrange
          grid::grid.draw(p)
          # p <- ggplot(temp, aes(y = y, x = x)) +
          #   labs(title = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(temp)[i]
          #                       significance$P_Value[i],")"), x = names(x)[i]) + # colnames(x)[i])
          #   geom_bar(position = "dodge", stat = "summary", fun.y = "mean") +
          #   scale_y_continuous(breaks = pretty_breaks(6))
        }
      } else {
        # y: numeric   x: numeric
        # calculate significance ANOVA
        significance$P_Value[i] <-
          round(summary(aov(temp$y ~ temp$x))[[1]][["Pr(>F)"]][[1]], rounding)
        # calculate bins for x
        binned <- temp %>%
          mutate(Group = cut(x, breaks = pretty(x, 40), include.lowest = T)) %>%
          group_by(Group) %>%
          summarise(y = mean(y),
                    x = median(x),
                    Count = n())
        # temp$Group <- cut(temp$x, breaks = pretty(temp$x, 20), include.lowest = T)
        if (length(unique(temp$x)) < 30) {
          if (!summary) {
            p <- qplot(data = temp, y = y, x = x, size = Freq,
                       main = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(temp)[i]
                                     significance$P_Value[i],")"), xlab = names(x)[i]) +
              geom_smooth(method = "loess") + # colnames(x)[i])
              scale_x_continuous(breaks = pretty_breaks(6)) +
              scale_y_continuous(breaks = pretty_breaks(6))
          } else {
            p1 <- ggplot(binned, aes(x = x, y = y)) +
              geom_line(size = I(1)) + geom_point(size = 2) +
              # geom_bar(stat = "identity", position = "dodge") +
              labs(title = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(x)[i]
                                  significance$P_Value[i],")"),
                   x = names(x)[i]) +
              scale_y_continuous(breaks = pretty_breaks(6))
            ifelse(SR_is_date(temp$x),
                   p1 <- p1 + scale_x_date(date_breaks = "year", date_labels = "%Y",
                                           date_minor_breaks = "month"),
                   p1 <- p1 + scale_x_continuous(breaks = pretty_breaks(8)))
            p2 <- ggplot(binned, aes(x = x, y = Count)) +
              geom_bar(stat = "identity", position = "dodge") +
              scale_y_continuous(breaks = pretty_breaks(4)) +
              labs(title = "", x = names(x)[i])
            p <- arrangeGrob(p1, p2, ncol = 1, heights = c(0.75, 0.25))   # grid.arrange
            grid::grid.draw(p)
            # p <- ggplot(temp, aes(y = y, x = x, size = Freq)) +
            #   labs(title = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(x)[i]
            #                       significance$P_Value[i],")"),
            #        x = names(x)[i]) +
            #   geom_smooth(method = "loess") +
            #   scale_x_continuous(breaks = pretty_breaks(6)) +
            #   scale_y_continuous(breaks = pretty_breaks(6))
          }
        } else {
          if (!summary) {
            p <- qplot(data = temp, y = y, x = x,
                       main = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(x)[i]
                                     significance$P_Value[i],")"),
                       xlab = names(x)[i]) +
              geom_smooth(method = "loess") +
              scale_x_continuous(breaks = pretty_breaks(6)) +
              scale_y_continuous(breaks = pretty_breaks(6))
          } else {
            p1 <- ggplot(binned, aes(x = x, y = y)) +
              geom_line(size = I(1)) + geom_point(size = 2) +
              # geom_bar(stat = "identity", position = "dodge") +
              labs(title = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(x)[i]
                                  significance$P_Value[i],")"),
                   x = names(x)[i]) +
              scale_y_continuous(breaks = pretty_breaks(6))
            ifelse(SR_is_date(temp$x),
                   p1 <- p1 + scale_x_date(date_breaks = "year", date_labels = "%Y",
                                           date_minor_breaks = "month"),
                   p1 <- p1 + scale_x_continuous(breaks = pretty_breaks(8)))
            p2 <- ggplot(binned, aes(x = x, y = Count)) +
              geom_bar(stat = "identity", position = "dodge") +
              scale_y_continuous(breaks = pretty_breaks(4)) +
              labs(title = "", x = names(x)[i])
            p <- arrangeGrob(p1, p2, ncol = 1, heights = c(0.75, 0.25))   # grid.arrange
            grid::grid.draw(p)
            # p <- ggplot(temp, aes(y = y, x = x)) +
            #   labs(title = paste0(i, ".) ", names(x)[i], " (p-Wert=", # colnames(x)[i]
            #                       significance$P_Value[i],")"),
            #        x = names(x)[i]) +
            #   geom_smooth(method = "loess") +
            #   scale_x_continuous(breaks = pretty_breaks(6)) +
            #   scale_y_continuous(breaks = pretty_breaks(6))
          }
        }
      }
    }
    if (!summary) {print(p)}
    if (save) {
      graphic_name <- paste0(path_output, "Univariate/", i, ". ", y_name, " ~ ", names(x)[i], ".png")
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
  q <- qplot(data = significance, y = reorder(Var, -P_Value), x = P_Value, ylab = "Variable",
             xlim = c(0, 1))
  print(q)
  if (save) {
    ggsave(paste0(path_output, "/Univariate/0. All Variables.png"), plot = q, width = 9.92, height = 5.3)  # 4.67
    write.table(significance, file = paste0(path_output, "/Univariate/0. All Variables.csv"),
                row.names = F, col.names = T, append = F, sep = ";", dec = ",")
  }
}
