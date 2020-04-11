test_that("characters", {
  expect_equal(SR_modus(c("a", "b", "b", "c")),
               "b")
})

test_that("numbers", {
  expect_equal(SR_modus(c(1, 2, 3, 3)),
               "3")
})

test_that("characters, method = 2", {
  expect_equal(SR_modus(c("a", "b", "b", "c"), method = 2),
               "b")
})

test_that("characters, method = 3", {
  expect_equal(SR_modus(c("a", "b", "b", "c"), method = 3),
               "b")
})
