data("mtcars")

test_that("y numeric", {
  expect_equal(SR_univariate_analysis(mtcars %>%
                                        dplyr::select(hp, cyl, mpg) %>%
                                        dplyr::mutate(cyl = as.factor(cyl)),
                                      y_name = "mpg", save = FALSE),
               NULL)
})

test_that("y factor", {
  expect_equal(SR_univariate_analysis(mtcars %>%
                                        dplyr::select(hp, cyl, mpg, am) %>%
                                        dplyr::mutate(cyl = as.factor(cyl),
                                                      am = as.factor(am)),
                                      y_name = "cyl", save = FALSE),
               NULL)
})
