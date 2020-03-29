test_that("conversion of 12:30:05", {
  expect_equal(round(SR_time_to_numeric("12:30:05"), 5),
               12.50139)
})

test_that("conversion of 00:00", {
  expect_equal(SR_time_to_numeric("00:00:00"),
               0)
})
