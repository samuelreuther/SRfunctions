data("mtcars")

test_that("Beispiel: einfach", {
  expect_equal(SR_plot_avg_and_count_per_group(mtcars,
                                               column_name = "hp", group_cat = "gear"),
               NULL)
})

test_that("Beispiel: viele Kategorien", {
  expect_equal(SR_plot_avg_and_count_per_group(mtcars %>%
                                                 dplyr::mutate(hp = round(hp)),
                                               column_name = "mpg", group_cat = "hp"),
               NULL)
})

test_that("Beispiel: berechnete Statistiken", {
  expect_equal(SR_plot_avg_and_count_per_group(df = mtcars %>%
                                                 dplyr::group_by(gear) %>%
                                                 dplyr::summarise(AVG = mean(hp, na.rm = TRUE),
                                                                  AVG = round(AVG, 2),
                                                                  COUNT = dplyr::n()),
                                               column_name = "hp", group_cat = "gear",
                                               calculate_stats = FALSE),
               NULL)
})

test_that("Beispiel: Anteile in Prozent", {
  expect_equal(SR_plot_avg_and_count_per_group(df = mtcars %>%
                                                 dplyr::group_by(gear) %>%
                                                 dplyr::summarise(AVG = mean(hp >= 100, na.rm = TRUE),
                                                                  AVG = round(AVG, 3),
                                                                  COUNT = dplyr::n()),
                                               column_name = "hp >= 100", group_cat = "gear",
                                               calculate_stats = FALSE),
               NULL)
})

test_that("Beispiel: group_cat ist ein factor", {
  expect_equal(SR_plot_avg_and_count_per_group(df = mtcars %>%
                                                 dplyr::mutate(hp = as.character(round(hp))) %>%
                                                 dplyr::group_by(hp) %>%
                                                 dplyr::summarise(AVG = mean(mpg, na.rm = TRUE),
                                                                  AVG = round(AVG, 1),
                                                                  COUNT = dplyr::n()),
                                               column_name = "mpg", group_cat = "hp",
                                               calculate_stats = FALSE),
               NULL)
})

