test_that("recognizes numerics", {
  expect_equal(SR_is_number(42.2),
               T)
})

test_that("recognizes integer", {
  expect_equal(SR_is_number(42L),
               T)
})

test_that("recognizes date", {
  expect_equal(SR_is_number(Sys.Date()),
               T)
})

test_that("recognizes non numbers", {
  expect_equal(SR_is_number("abc"),
               F)
})
