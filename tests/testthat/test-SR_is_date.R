test_that("recognize date", {
  expect_equal(SR_is_date(Sys.Date()),
               T)
})

test_that("recognize non date", {
  expect_equal(SR_is_date("asdf"),
               F)
})
