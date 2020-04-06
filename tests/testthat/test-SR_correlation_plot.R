data("mtcars")

test_that("class", {
  expect_equal(class(SR_correlation_plot(df = mtcars)),
               "matrix")
})
