df <- data.frame(var_numeric1 = c(-1, 0, 2, 3, NA, 4),
                 var_numeric2 = c(-5, 0, -2, NA, 3, 4),
                 var_character = c("a", NA, "b", "c", "c", "d"),
                 var_factor = factor(c("a", "b", "c", "c", "d", NA)),
                 var_date = seq.Date(as.Date("2020-04-12"), by = "day", length.out = 6),
                 stringsAsFactors = FALSE)

df_res1 <- data.frame(var_numeric1 = c(-1, 0, 2, 3, NA, 4),
                      var_numeric2 = c(-5, 0, -2, NA, 3, 4),
                      var_character = c("a", NA, "b", "c", "c", "d"),
                      var_factor = factor(c("a", "b", "c", "c", "d", NA)),
                      var_date = seq.Date(as.Date("2020-04-12"), by = "day", length.out = 6),
                      row_count_na = c(0, 1, 0, 1, 1, 1),
                      row_count_zero = c(0, 2, 0, 0, 0, 0),
                      row_count_negative = c(2, 0, 1, 0, 0, 0),
                      row_min = c(-5, 0, -2, 3, 3, 4),
                      row_mean = c(-3, 0, 0, 3, 3, 4),
                      row_max = c(-1, 0, 2, 3, 3, 4),
                      stringsAsFactors = FALSE)

test_that("class", {
  expect_equal(class(SR_feat_eng_rows(df)), "data.frame")
})

test_that("result", {
  expect_equal(SR_feat_eng_rows(df), df_res1)
})

rm(df, df_res1)
