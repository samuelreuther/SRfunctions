test_that("vector with numbers works", {
  expect_equal(SR_omit_non_regular_values(as.vector(c(1, Inf, NaN, NA, -Inf))),
               1)
})

test_that("vector with characters works", {
  expect_equal(SR_omit_non_regular_values(as.vector(c("a", NaN, NA))),
               "a")
})

test_that("factor works", {
  expect_equal(SR_omit_non_regular_values(as.factor(c("a", Inf, NaN, NA, -Inf))),
               as.factor("a"))
})

test_that("data.frame works", {
  expect_equal(SR_omit_non_regular_values(data.frame(a = c(1, Inf, NaN, NA, -Inf),
                                                     b = 1:5)),
               data.frame(a = 1, b = 1))
})

test_that("matrix works", {
  expect_equal(SR_omit_non_regular_values(matrix(data = c(1, Inf, NaN, NA, -Inf, 1:5),
                                                 ncol = 2)),
               matrix(data = c(1, 1), ncol = 2))
})
