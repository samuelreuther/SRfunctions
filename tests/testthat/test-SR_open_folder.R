test_that("missing folder gives error", {
  expect_error(SR_open_folder_in_pane("does/not/exist"), "Folder does not exist")
  skip_on_os(c("mac", "linux", "solaris"))
  expect_error(SR_open_folder_in_explorer("does/not/exist"), "Folder does not exist")
})
