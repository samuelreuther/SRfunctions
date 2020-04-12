df <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 4),
                 var_character = c("a", NA, "b", "c", "c", "d"),
                 var_factor = factor(c("a", "b", "c", "c", "d", NA)),
                 var_date = seq.Date(as.Date("2020-04-12"), by = "day", length.out = 6),
                 stringsAsFactors = FALSE)

df_res1 <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 4),
                      var_character = c("a", NA, "b", "c", "c", "d"),
                      var_factor = factor(c("a", "b", "c", "c", "d", NA)),
                      var_date = c(20200412, 20200413, 20200414, 20200415,
                                   20200416, 20200417),
                      stringsAsFactors = FALSE)

df_res2 <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 4),
                      var_character = c("a", NA, "b", "c", "c", "d"),
                      var_factor = factor(c("a", "b", "c", "c", "d", NA)),
                      var_date = c(20200412, 20200413, 20200414, 20200415,
                                   20200416, 20200417),
                      var_date_month = c(4, 4, 4, 4, 4, 4),
                      var_date_day = c(12, 13, 14, 15, 16, 17),
                      var_date_weekday = c("Sonntag", "Montag", "Dienstag", "Mittwoch",
                                           "Donnerstag", "Freitag"),
                      stringsAsFactors = FALSE)

test_that("class", {
  expect_equal(class(SR_feat_eng_date(df)), "data.frame")
})

test_that("result", {
  expect_equal(SR_feat_eng_date(df), df_res1)
})

test_that("result, only_date_to_numeric = FALSE", {
  expect_equal(SR_feat_eng_date(df, only_date_to_numeric = FALSE), df_res2)
})

rm(df, df_res1, df_res2)
