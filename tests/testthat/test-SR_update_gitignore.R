test_that("returns invisibly on NULL input", {
  expect_invisible(SR_update_gitignore())
  expect_null(SR_update_gitignore())
})

test_that("returns invisibly when source file does not exist", {
  tmp_dir <- tempdir()
  expect_invisible(SR_update_gitignore(from = file.path(tmp_dir, "nonexistent"),
                                       to   = tmp_dir))
})

test_that("returns invisibly when target directory does not exist", {
  tmp_from <- tempfile()
  writeLines(c("*.log", "*.tmp"), tmp_from)
  expect_invisible(SR_update_gitignore(from = tmp_from,
                                       to   = file.path(tempdir(), "nonexistent_dir")))
  file.remove(tmp_from)
})

test_that("appends only missing patterns", {
  tmp_dir <- file.path(tempdir(), "gitignore_test_append")
  dir.create(tmp_dir, showWarnings = FALSE)
  #
  # source has three patterns
  tmp_from <- file.path(tmp_dir, "source_gitignore")
  writeLines(c("*.log", "*.tmp", ".DS_Store"), tmp_from)
  #
  # target already has one of them
  tmp_to_file <- file.path(tmp_dir, ".gitignore")
  writeLines(c("# my rules", "*.log"), tmp_to_file)
  #
  SR_update_gitignore(from = tmp_from, to = tmp_dir)
  #
  result <- readLines(tmp_to_file)
  result_patterns <- trimws(result[nchar(trimws(result)) > 0 & !grepl("^\\s*#", result)])
  #
  expect_true("*.log"    %in% result_patterns)
  expect_true("*.tmp"    %in% result_patterns)
  expect_true(".DS_Store" %in% result_patterns)
  # original pattern not duplicated
  expect_equal(sum(result_patterns == "*.log"), 1)
  #
  unlink(tmp_dir, recursive = TRUE)
})

test_that("no-op when target already contains all patterns", {
  tmp_dir <- file.path(tempdir(), "gitignore_test_noop")
  dir.create(tmp_dir, showWarnings = FALSE)
  #
  tmp_from <- file.path(tmp_dir, "source_gitignore")
  writeLines(c("*.log", "*.tmp"), tmp_from)
  #
  tmp_to_file <- file.path(tmp_dir, ".gitignore")
  writeLines(c("*.log", "*.tmp"), tmp_to_file)
  original_content <- readLines(tmp_to_file)
  #
  SR_update_gitignore(from = tmp_from, to = tmp_dir)
  #
  expect_equal(readLines(tmp_to_file), original_content)
  #
  unlink(tmp_dir, recursive = TRUE)
})

test_that("updates .gitignore files in subdirectories", {
  tmp_dir <- file.path(tempdir(), "gitignore_test_recursive")
  sub_dir <- file.path(tmp_dir, "subproject")
  dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  #
  tmp_from <- file.path(tmp_dir, "source_gitignore")
  writeLines(c("*.log", ".DS_Store"), tmp_from)
  #
  # root .gitignore already complete, subproject is missing one entry
  writeLines(c("*.log", ".DS_Store"), file.path(tmp_dir, ".gitignore"))
  writeLines(c("*.log"),              file.path(sub_dir, ".gitignore"))
  #
  SR_update_gitignore(from = tmp_from, to = tmp_dir)
  #
  sub_patterns <- trimws(readLines(file.path(sub_dir, ".gitignore")))
  sub_patterns <- sub_patterns[nchar(sub_patterns) > 0 & !grepl("^#", sub_patterns)]
  expect_true(".DS_Store" %in% sub_patterns)
  #
  unlink(tmp_dir, recursive = TRUE)
})

test_that("does not modify source file when it lives inside the target directory", {
  tmp_dir <- file.path(tempdir(), "gitignore_test_no_self")
  sub_dir <- file.path(tmp_dir, "subproject")
  dir.create(sub_dir, recursive = TRUE, showWarnings = FALSE)
  #
  # source .gitignore is inside the target tree
  src_file <- file.path(tmp_dir, ".gitignore")
  writeLines(c("*.log", ".DS_Store"), src_file)
  original_content <- readLines(src_file)
  #
  writeLines(c("*.log"), file.path(sub_dir, ".gitignore"))
  #
  SR_update_gitignore(from = src_file, to = tmp_dir)
  #
  # source file must be unchanged
  expect_equal(readLines(src_file), original_content)
  # subproject should have been updated
  sub_patterns <- trimws(readLines(file.path(sub_dir, ".gitignore")))
  sub_patterns <- sub_patterns[nchar(sub_patterns) > 0 & !grepl("^#", sub_patterns)]
  expect_true(".DS_Store" %in% sub_patterns)
  #
  unlink(tmp_dir, recursive = TRUE)
})

test_that("ignores comment lines when diffing", {
  tmp_dir <- file.path(tempdir(), "gitignore_test_comments")
  dir.create(tmp_dir, showWarnings = FALSE)
  #
  tmp_from <- file.path(tmp_dir, "source_gitignore")
  writeLines(c("# OS files", ".DS_Store", "*.log"), tmp_from)
  #
  tmp_to_file <- file.path(tmp_dir, ".gitignore")
  writeLines(c("# different comment", ".DS_Store"), tmp_to_file)
  #
  SR_update_gitignore(from = tmp_from, to = tmp_dir)
  #
  result <- readLines(tmp_to_file)
  result_patterns <- trimws(result[nchar(trimws(result)) > 0 & !grepl("^\\s*#", result)])
  #
  expect_true("*.log"    %in% result_patterns)
  expect_true(".DS_Store" %in% result_patterns)
  expect_equal(sum(result_patterns == ".DS_Store"), 1)
  #
  unlink(tmp_dir, recursive = TRUE)
})

