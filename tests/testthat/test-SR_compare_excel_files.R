skip_if_not_installed("writexl")

# helper: write a named list of data.frames to a temp xlsx and return the path
write_xlsx_tmp <- function(sheets) {
  path <- tempfile(fileext = ".xlsx")
  writexl::write_xlsx(sheets, path)
  path
}

test_that("returns invisible TRUE for identical files", {
  df <- data.frame(a = 1:3, b = c("x", "y", "z"))
  f1 <- write_xlsx_tmp(list(Sheet1 = df))
  f2 <- write_xlsx_tmp(list(Sheet1 = df))
  expect_true(SR_compare_excel_files(f1, f2))
  file.remove(f1, f2)
})

test_that("returns invisible NULL and warns when sheet count differs", {
  f1 <- write_xlsx_tmp(list(Sheet1 = data.frame(a = 1), Sheet2 = data.frame(b = 2)))
  f2 <- write_xlsx_tmp(list(Sheet1 = data.frame(a = 1)))
  expect_output(result <- SR_compare_excel_files(f1, f2),
                "number of sheets are different")
  expect_null(result)
  file.remove(f1, f2)
})

test_that("returns invisible NULL and warns when sheet names differ", {
  f1 <- write_xlsx_tmp(list(SheetA = data.frame(a = 1)))
  f2 <- write_xlsx_tmp(list(SheetB = data.frame(a = 1)))
  expect_output(result <- SR_compare_excel_files(f1, f2),
                "names of the sheets are different")
  expect_null(result)
  file.remove(f1, f2)
})

test_that("reports differing columns when data differs", {
  f1 <- write_xlsx_tmp(list(Sheet1 = data.frame(a = 1:3, b = 1:3)))
  f2 <- write_xlsx_tmp(list(Sheet1 = data.frame(a = 1:3, b = c(1L, 2L, 99L))))
  expect_output(SR_compare_excel_files(f1, f2), "Difference in")
  file.remove(f1, f2)
})

test_that("handles multiple sheets, reports only differing ones", {
  same <- data.frame(x = 1:2)
  diff1 <- data.frame(x = 1:2)
  diff2 <- data.frame(x = c(1L, 99L))
  f1 <- write_xlsx_tmp(list(Same = same, Different = diff1))
  f2 <- write_xlsx_tmp(list(Same = same, Different = diff2))
  out <- capture.output(SR_compare_excel_files(f1, f2))
  expect_true(any(grepl("Different", out)))
  file.remove(f1, f2)
})
