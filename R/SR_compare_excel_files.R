#' Compare two Excel files
#'
#' Compares two Excel files sheet by sheet. Prints any differences found in
#' sheet names or cell values. Uses \code{compare::compare()} with
#' \code{allowAll = FALSE} so no implicit transformations are applied.
#'
#' @param file1 character: path to the first Excel file
#' @param file2 character: path to the second Excel file
#'
#' @return invisible(TRUE) if all sheets are identical, invisible(NULL) otherwise
#'
#' @examples
#' \dontrun{
#' SR_compare_excel_files(file1 = "report_v1.xlsx",
#'                        file2 = "report_v2.xlsx")
#' }
#'
#' @export
SR_compare_excel_files <- function(file1, file2) {
  #
  # get sheet names
  sheet_names   <- readxl::excel_sheets(file1)
  sheet_names_2 <- readxl::excel_sheets(file2)
  #
  # check sheet structure
  if (!identical(sheet_names, sheet_names_2)) {
    if (length(sheet_names) != length(sheet_names_2)) {
      cat("Warning: the number of sheets are different!\n")
      missing_in_2 <- sheet_names[!sheet_names %in% sheet_names_2]
      missing_in_1 <- sheet_names_2[!sheet_names_2 %in% sheet_names]
      if (length(missing_in_2) > 0)
        cat("This sheet is missing in excel file 2:", missing_in_2, "\n")
      if (length(missing_in_1) > 0)
        cat("This sheet is missing in excel file 1:", missing_in_1, "\n")
    } else {
      cat("Warning: the names of the sheets are different!\n",
          sheet_names[sheet_names != sheet_names_2], "vs.",
          sheet_names_2[sheet_names_2 != sheet_names])
    }
    return(invisible(NULL))
  }
  #
  # compare each sheet
  for (sheet in sheet_names) {
    suppressMessages({
      data1 <- readxl::read_excel(file1, sheet = sheet)
      data2 <- readxl::read_excel(file2, sheet = sheet)
    })
    #
    comparison <- compare::compare(data1, data2, allowAll = FALSE)
    #
    if (!isTRUE(comparison)) {
      cat("Difference in:\n", " sheet:", sheet, "\ncolumns:",
          comparison[["detailedResult"]][comparison[["detailedResult"]] == FALSE] %>%
            names(.) %>% paste0(., collapse = ", "), "\n")
      if (nrow(data1) == nrow(data2) & ncol(data1) == ncol(data2)) {
        cat(paste(comparison[["tM"]][comparison[["tM"]] != comparison[["tC"]]],
                  collapse = "\n"))
      }
    }
  }
  #
  return(invisible(TRUE))
}
