test_that("simple example", {
  expect_equal(SR_replace_NA_median(df = data.frame(a = c(1, 2, 2, 3, NA)),
                                    var = "a"),
               data.frame(a = c(1, 2, 2, 3, 2),
                          a_NA = c(0, 0, 0, 0, 1)))
})

test_that("use_other_df", {
  expect_equal(SR_replace_NA_median(df = data.frame(a = c(1, 2, 2, 3, NA)),
                                    use_other_df = data.frame(a = c(4, 5, 5, 6, 7))),
               data.frame(a = c(1, 2, 2, 3, 5),
                          a_NA = c(0, 0, 0, 0, 1)))
})
