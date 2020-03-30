data("mtcars")

test_that("x numeric, y numeric", {
  expect_equal(SR_plot_true_y_VS_predicted_y(true_y = mtcars$hp,
                                             predicted_y = mtcars$hp +
                                               rnorm(nrow(mtcars), sd = 30)),
               NULL)
})
