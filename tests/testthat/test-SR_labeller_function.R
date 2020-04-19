df <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 4),
                 var_character = c("a", NA, "b", "c", "c", "d"),
                 stringsAsFactors = FALSE)
df <- SR_labeller_function(df,
                           c(var_numeric = "a",
                             var_character = "b"))

test_that("class", {
  expect_equal(class(Hmisc::contents(df)), "contents.data.frame")
})

rm(df)
