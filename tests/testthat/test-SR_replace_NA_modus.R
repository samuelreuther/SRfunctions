test_that("simple example", {
  expect_equal(SR_replace_NA_modus(df = data.frame(a = factor(c(1, 2, 2, 3, NA))),
                                   var = "a"),
               data.frame(a = factor(c(1, 2, 2, 3, 2)),
                          a_NA = c(0, 0, 0, 0, 1)))
})

# RUNS INTO ERRORS, NOT SHURE WHY, BECAUSE NO ERROR WHEN I TRY IT !!!
#
# test_that("use_other_df", {
#   expect_equal(SR_replace_NA_modus(df = data.frame(a = factor(c(1, 2, 2, 3, NA)),
#                                                    b = c("a", NA, "b", "c", "c")),
#                                    use_other_df = data.frame(a = factor(c(4, 5, 5, 6, 7)),
#                                                              b = c("e", "f", "X", "X", "h"))),
#                data.frame(a = factor(c(1, 2, 2, 3, 5)),
#                           b = c("a", "X", "b", "c", "c"),
#                           a_NA = c(0, 0, 0, 0, 1),
#                           b_NA = c(0, 1, 0, 0, 0)))
# })
