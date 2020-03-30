data("mtcars")

test_that("multiplication works", {
  expect_equal(SR_univariate_analysis(mtcars %>%
                                        select(hp, cyl, mpg) %>%
                                        mutate(cyl = as.factor(cyl)),
                                      y_name = "mpg", save = F),
               NULL)
})
