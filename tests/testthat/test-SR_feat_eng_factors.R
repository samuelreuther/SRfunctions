df <- data.frame(var_character = c("a", NA, "b", "c", "c", "d"),
                 var_factor = factor(c("a", "b", "c", "c", "d", NA)),
                 stringsAsFactors = FALSE)

df_res1 <- data.frame(var_character_LabelEnc = c(1, 5, 2, 3, 3, 4),
                      var_factor_LabelEnc = (c(1, 2, 3, 3, 4, 5)),
                      stringsAsFactors = FALSE)

test_that("result", {
  expect_equal(SR_feat_eng_factors(df), df_res1)
})

test_that("class", {
  expect_equal(class(SR_feat_eng_rows(df)), "data.frame")
})

test_that("data.frame factor_encoding in memory", {
  expect_true(exists("factor_encoding"))
})

rm(df, df_res1)
