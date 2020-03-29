test_that("recognizes date", {
  expect_equal(SR_is_date(Sys.Date()),
               T)
})

test_that("recognizes non date", {
  expect_equal(SR_is_date("asdf"),
               F)
})
