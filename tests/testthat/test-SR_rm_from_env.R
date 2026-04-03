test_that("removes matching objects from global environment", {
  assign("tmp_a", 1, envir = .GlobalEnv)
  assign("tmp_b", 2, envir = .GlobalEnv)
  assign("keep_me", 3, envir = .GlobalEnv)
  #
  SR_rm_from_env(pattern = "^tmp_")
  #
  expect_false(exists("tmp_a", envir = .GlobalEnv, inherits = FALSE))
  expect_false(exists("tmp_b", envir = .GlobalEnv, inherits = FALSE))
  expect_true(exists("keep_me", envir = .GlobalEnv, inherits = FALSE))
  #
  rm("keep_me", envir = .GlobalEnv)
})

test_that("no error when pattern matches nothing", {
  expect_no_error(SR_rm_from_env(pattern = "^this_does_not_exist_xyz$"))
})
