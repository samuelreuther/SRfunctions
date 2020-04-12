df <- data.frame(var_character = c("ä", NA, "Ä", "ß", "ö", "Ö"),
                 var_factor = c("é", "è", "ü", "Ü", "d", NA),
                 stringsAsFactors = FALSE)

df_res <- data.frame(var_character = c("ae", NA, "Ae", "ss", "oe", "Oe"),
                 var_factor = c("e", "e", "ue", "Ue", "d", NA),
                 stringsAsFactors = FALSE)

test_that("result", {
  expect_equal(SR_replace_umlaute(df), df_res)
})

test_that("result, class", {
  expect_equal(class(SR_replace_umlaute(df)), "data.frame")
})

rm(df)
