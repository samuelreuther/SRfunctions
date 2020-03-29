test_that("recognize numerics", {
  expect_equal(SR_is_number(42.2),
               T)
})

test_that("recognize integer", {
  expect_equal(SR_is_number(42L),
               T)
})

test_that("recognize date", {
  expect_equal(SR_is_number(Sys.Date()),
               T)
})

test_that("recognize non numbers", {
  expect_equal(SR_is_number("abc"),
               F)
})
