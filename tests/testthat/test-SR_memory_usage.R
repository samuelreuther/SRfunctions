# test_that("class, no objects", {
#   expect_equal(class(SR_memory_usage()),
#                "NULL")
# })

df <- data.frame(var_numeric = c(1, 2, 2, 3, 3, 4),
                 var_character = c("a", NA, "b", "c", "c", "d"))

test_that("class, 1 data.frame", {
  expect_equal(class(SR_memory_usage()),
               "data.frame")
})

rm(df)
