test_that("class of plot", {
  expect_equal(class(SR_mosaicplot(df = data.frame(y_true = c(0, 0, 0, 1, 1, 1),
                                                   predicted_y = c(0.8, 0.1, 0.4, 0.2, 0.9, 0.8)),
                                   cutoff = 0.5))[1],
               "gg")
})
