df <- data.frame(var_numeric = c(1, 2, 2, 3, 3, 4),
                 var_character = c("a", NA, "b", "c", "c", "d"),
                 var_factor = factor(c("a", "b", "c", "c", "d", NA)),
                 var_date = seq.Date(Sys.Date(), by = "day", length.out = 6),
                 stringsAsFactors = FALSE)

test_that("class", {
  expect_equal(class(SR_visualize_df(df)), "NULL")
})

rm(df)
