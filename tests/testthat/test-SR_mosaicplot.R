test_that("class of plot", {
  expect_equal(class(SR_mosaicplot(var1 = c(0, 0, 0, 1, 1, 1),
                                   var2 = c(1, 0, 0, 0, 1, 1)))[1],
               "gg")
})

