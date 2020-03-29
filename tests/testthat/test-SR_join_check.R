test_that("function works without error", {
  expect_equal(SR_join_check(data.frame(a = 1:3,
                                        b = 1:3),
                             data.frame(a = c(3, 3:5),
                                        c = 1:4)),
               NULL)
})
