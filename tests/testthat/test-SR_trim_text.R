test_that("removing leading and trailing whitespace works", {
  expect_equal(SR_trim_text(" abc "),
               "abc")
})

test_that("does not remove other text", {
  expect_equal(SR_trim_text("def"),
               "def")
})
