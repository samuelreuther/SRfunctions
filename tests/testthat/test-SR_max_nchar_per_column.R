df <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 44),
                 var_character = c("abc", NA, "bd", "cdefg", "c", "ef"),
                 stringsAsFactors = FALSE)

result <- data.frame(column = c("var_numeric", "var_character"),
                     max_nchar = c(2, 5),
                     stringsAsFactors = FALSE)

test_that("result", {
  expect_equal(SR_max_nchar_per_column(df), result)
})
