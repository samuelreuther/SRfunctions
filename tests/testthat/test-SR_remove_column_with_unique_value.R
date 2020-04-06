test_that("class", {
  expect_equal(class(SR_remove_column_with_unique_value(
    df = data.frame(numeric = c(0, NA, 0.3, 0.5, NA, 1),
                    character = c("a", "b", NA, "d", NA, "f"),
                    integer = c(0L, NA, NA, NA, NA, NA),
                    remove = c(1, 1, 1, 1, 1, 1)))),
    "data.frame")
})

test_that("class, silent", {
  expect_equal(class(SR_remove_column_with_unique_value(
    df = data.frame(numeric = c(0, NA, 0.3, 0.5, NA, 1),
                    character = c("a", "b", NA, "d", NA, "f"),
                    integer = c(0L, NA, NA, NA, NA, NA),
                    remove = c(1, 1, 1, 1, 1, 1)),
    silent = T)),
    "data.frame")
})

test_that("ncol, remove 1 column", {
  expect_equal(ncol(SR_remove_column_with_unique_value(
    df = data.frame(numeric = c(0, NA, 0.3, 0.5, NA, 1),
                    character = c("a", "b", NA, "d", NA, "f"),
                    integer = c(0L, NA, NA, NA, NA, NA),
                    remove = c(1, 1, 1, 1, 1, 1)))),
    3)
})

test_that("ncol, remove_na = T", {
  expect_equal(ncol(SR_remove_column_with_unique_value(
    df = data.frame(numeric = c(0, NA, 0.3, 0.5, NA, 1),
                    character = c("a", "b", NA, "d", NA, "f"),
                    integer = c(0L, NA, NA, NA, NA, NA),
                    remove = c(1, 1, 1, 1, 1, 1)),
    remove_na = T)),
    2)
})
