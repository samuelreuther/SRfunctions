test_that("function works without error", {
  expect_equal(SR_join_check(data.frame(a = 1:15,
                                        b = 1:15,
                                        d = rep(1, 15)), # skiped
                             data.frame(a = 5:24,
                                        c = 1:20,
                                        d = rep(1, 20))), # skiped
               NULL)
})

