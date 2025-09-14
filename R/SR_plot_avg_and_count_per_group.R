#' Plot y by x
#'
#' Generates different univariate plots, depending if x/y are numeric, or factor/character
#'
#' @param df data.frame
#' @param column_name character
#' @param group_cat character
#' @param calculate_stats Boolean (default: TRUE ) If FALSE then provide DF with
#'                        the following columns: group_cat, AVG, COUNT
#' @param title character (default: NULL) provide optionally a title
#' @param save Boolean
#' @param path_output character
#'
#' @return ggplot
#'
#' @examples
#' data("mtcars")
#' # Beispiel: einfach
#' SR_plot_avg_and_count_per_group(mtcars,
#'                                 column_name = "hp", group_cat = "gear")
#' # Beispiel: viele Kategorien
#' SR_plot_avg_and_count_per_group(mtcars %>%
#'                                   dplyr::mutate(hp = round(hp)),
#'                                 column_name = "mpg", group_cat = "hp")
#' # Beispiel: berechnete Statistiken
#' SR_plot_avg_and_count_per_group(df = mtcars %>%
#'                                        dplyr::group_by(gear) %>%
#'                                        dplyr::summarise(AVG = mean(hp, na.rm = TRUE),
#'                                                         AVG = round(AVG, 2),
#'                                                         COUNT = dplyr::n()),
#'                                 column_name = "hp", group_cat = "gear",
#'                                 calculate_stats = FALSE)
#' # Beispiel: Anteile in Prozent
#' SR_plot_avg_and_count_per_group(df = mtcars %>%
#'                                        dplyr::group_by(gear) %>%
#'                                        dplyr::summarise(AVG = mean(hp >= 100, na.rm = TRUE),
#'                                                         AVG = round(AVG, 3),
#'                                                         COUNT = dplyr::n()),
#'                                 column_name = "hp >= 100", group_cat = "gear",
#'                                 calculate_stats = FALSE)
#' # Beispiel: group_cat ist ein factor
#' SR_plot_avg_and_count_per_group(df = mtcars %>%
#'                                        dplyr::mutate(hp = as.character(round(hp))) %>%
#'                                        dplyr::group_by(hp) %>%
#'                                        dplyr::summarise(AVG = mean(mpg, na.rm = TRUE),
#'                                                         AVG = round(AVG, 1),
#'                                                         COUNT = dplyr::n()),
#'                                 column_name = "mpg", group_cat = "hp",
#'                                 calculate_stats = FALSE)
#
#' @export
SR_plot_avg_and_count_per_group <- function(df,
                                            column_name, group_cat,
                                            calculate_stats = TRUE,
                                            title = NULL,
                                            save = FALSE,
                                            path_output = getwd()) {
  ### calculate stats
  #
  if (calculate_stats) {
    df_stats <- df %>%
      dplyr::group_by(.data[[group_cat]]) %>%
      dplyr::summarise(AVG = mean(.data[[column_name]], na.rm = TRUE),
                       COUNT = dplyr::n()) %>%
      dplyr:: mutate(AVG = dplyr::case_when(
        abs(median(AVG, na.rm = TRUE)) >= 1000     ~ round(AVG, 0),
        abs(median(AVG, na.rm = TRUE)) >=  100     ~ round(AVG, 1),
        abs(median(AVG, na.rm = TRUE)) >=   10     ~ round(AVG, 2),
        abs(median(AVG, na.rm = TRUE)) >=    1     ~ round(AVG, 3),
        abs(median(AVG, na.rm = TRUE)) >=    0.1   ~ round(AVG, 4),
        abs(median(AVG, na.rm = TRUE)) >=    0.01  ~ round(AVG, 5),
        abs(median(AVG, na.rm = TRUE)) >=    0.001 ~ round(AVG, 6)))
  } else {
    df_stats <- df
  }
  #
  #
  #
  ### calculate scale
  #
  scale <- max(df_stats$COUNT) / max(df_stats$AVG)
  scale <- dplyr::if_else(scale < 0.5, scale, round(scale))
  scale <- dplyr::case_when(round(scale, -nchar(scale)) == 0 & round(scale) > 0 ~
                              round(scale, -nchar(scale) + 1),
                            round(scale, -nchar(scale)) > 0  & round(scale) > 0 ~
                              round(scale, -nchar(scale)),
                            round(scale * 10)    > 0 ~ round(scale, 1),
                            round(scale * 100)   > 0 ~ round(scale, 2),
                            round(scale * 1000)  > 0 ~ round(scale, 3),
                            round(scale * 10000) > 0 ~ round(scale, 4),
                            T                        ~ scale)
  #
  #
  #
  ### plot
  #
  p <- ggplot2::ggplot(df_stats,
                       ggplot2::aes(x = .data[[group_cat]], y = COUNT)) +
    # aes(x = {{group_cat}}, y = COUNT)) +
    ggplot2::geom_bar(stat = "identity", fill = "grey") +
    {if (nrow(df_stats) <= 20)
      ggplot2::geom_text(ggplot2::aes(y = COUNT / 2,
                                      label = format(COUNT, big.mark = "'")),
                         size = 3)} +
    ggplot2::geom_line(ggplot2::aes(y = (AVG) * scale, group = 1),
                       linewidth = 1, colour = "steelblue") +
    {if (nrow(df_stats) <= 20)
      ggplot2::geom_label(ggplot2::aes(y = (AVG) * scale, label = AVG),
                          size = 4, fill = "steelblue")} +
    {if (nrow(df_stats) > 20)
      ggplot2::geom_point(ggplot2::aes(y = (AVG) * scale),
                          size = 4, colour = "steelblue")} +
    {if (class(df_stats[[group_cat]]) %in% c("character", "factor"))
      ggplot2::scale_x_discrete(labels = scales::wrap_format(20))} +
    # {if (!class(df_stats[[group_cat]]) %in% c("character", "factor"))
    #   ggplot2::scale_x_continuous(labels = scales::pretty_breaks(6))} +  # not working because of calculate_stats
    ggplot2::scale_y_continuous(breaks = scales::pretty_breaks(6),
                                name = "Anzahl Daten (Säule)",
                                labels = scales::format_format(big.mark = "'"),
                                sec.axis =
                                  ggplot2::sec_axis(transform = ~. / scale,
                                                    name = paste0("Durchschnittswerte von '",
                                                                  column_name, "' (blau)"),
                                                    breaks = scales::pretty_breaks(6),
                                                    labels = scales::format_format(big.mark = "'"))) +
    ggplot2::labs(title = ifelse(is.null(title),
                                 paste0("Anzahlen (graue Säule) und ",
                                        "Durchschnittswerte (blau) von '",
                                        column_name, "' pro Kategorie von '",
                                        group_cat),
                                 title),
                  x = group_cat)
  #
  #
  #
  # save graphic
  if (save) ggplot2::ggsave(paste0(path_output, "Avg_", column_name, "_per_",
                                   group_cat, ".png"),
                            plot = p, width = 9.92, height = 5.3)
  #
  # return result
  print(p)
  return(invisible(NULL))
}

