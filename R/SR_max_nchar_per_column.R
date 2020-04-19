#' Determine the maximum nchar for each column of a data.frame
#'
#' Determine the maximum nchar for each column of a data.frame.
#'
#' @param df data.frame
#'
#' @return data.frame
#'
#' @examples
#' df <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 44),
#'                  var_character = c("abc", NA, "bd", "cdefg", "c", "ef"),
#'                  stringsAsFactors = FALSE)
#' SR_max_nchar_per_column(df)
#'
#' @export
SR_max_nchar_per_column <- function(df) {
  suppressWarnings(lapply(df, function(x) max(nchar(x), na.rm = T)) %>%
                     unlist() %>%
                     data.frame() %>%
                     tibble::rownames_to_column(var = "column") %>%
                     dplyr::rename("max_nchar" = 2))
}
