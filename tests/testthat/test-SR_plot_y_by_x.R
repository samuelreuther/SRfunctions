data("mtcars")

test_that("x numeric, y numeric", {
  expect_equal(SR_plot_y_by_x(mtcars,
                              x = "hp", y = "mpg"),
               NULL)
})

test_that("x factor, y numeric", {
  expect_equal(SR_plot_y_by_x(mtcars %>% mutate(hp = as.factor(hp)),
                              x = "hp", y = "mpg"),
               NULL)
})
