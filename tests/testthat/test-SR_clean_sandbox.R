test_that("returns invisibly when path does not exist", {
  expect_invisible(SR_clean_sandbox(path = "/nonexistent/path"))
})

test_that("removes Rplots.pdf, hs_err_pid*.log, and .ipynb_checkpoints", {
  tmp <- file.path(tempdir(), "sandbox_test")
  fns_dir <- file.path(tmp, "Functions")
  sub_dir <- file.path(tmp, "project")
  dir.create(fns_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(sub_dir, showWarnings = FALSE)
  #
  # create artifacts
  file.create(file.path(tmp, "Rplots.pdf"))
  file.create(file.path(sub_dir, "hs_err_pid12345.log"))
  dir.create(file.path(sub_dir, ".ipynb_checkpoints"), showWarnings = FALSE)
  # create a regular file that must NOT be removed
  file.create(file.path(sub_dir, "analysis.R"))
  #
  removed <- SR_clean_sandbox(path = tmp)
  #
  expect_false(file.exists(file.path(tmp, "Rplots.pdf")))
  expect_false(file.exists(file.path(sub_dir, "hs_err_pid12345.log")))
  expect_false(dir.exists(file.path(sub_dir, ".ipynb_checkpoints")))
  expect_true(file.exists(file.path(sub_dir, "analysis.R")))
  expect_equal(length(removed), 3L)
  #
  unlink(tmp, recursive = TRUE)
})

test_that("writes timestamp rds to hardcoded path after cleanup", {
  ts_file <- "/home/sandbox/sandbox/Functions/.last_cleanup.rds"
  skip_if_not(dir.exists(dirname(ts_file)),
              "Hardcoded timestamp path not available in this environment")
  #
  tmp <- file.path(tempdir(), "sandbox_test_ts")
  dir.create(file.path(tmp, "Functions"), recursive = TRUE, showWarnings = FALSE)
  #
  SR_clean_sandbox(path = tmp)
  #
  expect_true(file.exists(ts_file))
  expect_s3_class(readRDS(ts_file), "POSIXct")
  #
  unlink(tmp, recursive = TRUE)
})

test_that("returns invisibly with zero-length vector when nothing to remove", {
  tmp <- file.path(tempdir(), "sandbox_test_empty")
  dir.create(file.path(tmp, "Functions"), recursive = TRUE, showWarnings = FALSE)
  #
  result <- SR_clean_sandbox(path = tmp)
  expect_length(result, 0)
  #
  unlink(tmp, recursive = TRUE)
})
