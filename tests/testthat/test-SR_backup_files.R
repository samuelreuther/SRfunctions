test_that("class", {
  expect_equal(class(SR_backup_files(path_from = getwd(),
                                     path_to = paste0(getwd(), "/my_backup_folder/"),
                                     include = c("license"),
                                     exclude = c("backup"))),
               "NULL")
})

unlink(paste0(getwd(), "/my_backup_folder"), recursive = TRUE, force = TRUE)
