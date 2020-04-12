df <- data.frame(var_numeric1 = c(1, NA, 2, 3, 3, 4),
                 var_numeric2 = c(1, 2, 3, 3, 4, NA))

df_res1 <- data.frame(var_numeric1 = c(1, 3, 2, 3, 3, 4),
                      var_numeric2 = c(1, 2, 3, 3, 4, 3),
                      var_numeric1_NA = c(0, 1, 0, 0, 0, 0),
                      var_numeric2_NA = c(0, 0, 0, 0, 0, 1))

test_that("result", {
  expect_equal(SR_feat_eng_numeric(df, replace_NA_median = TRUE), df_res1)
})

test_that("class", {
  expect_equal(class(SR_feat_eng_numeric(df)), "data.frame")
})

rm(df, df_res1)
