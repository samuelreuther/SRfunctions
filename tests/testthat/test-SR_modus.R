test_that("characters", {
  expect_equal(SR_modus(c("a", "b", "b", "c")),
               "b")
})

test_that("numbers", {
  expect_equal(SR_modus(c(1, 2, 3, 3)),
               "3")
})
