test_that("class", {
  expect_equal(class(SR_NA_plot(df = data.frame(numeric = c(0, NA, 0.3, 0.5, NA, 1),
                                                character = c("a", "b", NA, "d", NA, "f"),
                                                integer = c(0L, NA, 3L, 5L, NA, 1L))))[1],
               "matrix")
})

