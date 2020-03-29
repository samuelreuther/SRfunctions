test_that("end of year", {
  expect_equal(SR_end_of_month("2019-12-24"),
               as.Date("2019-12-31"))
})

test_that("end of february in a lear year", {
  expect_equal(SR_end_of_month("2020-02-22"),
               as.Date("2020-02-29"))
})
