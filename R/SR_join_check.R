#' Join-Check
#'
#' Check the number of rows after different joining methods of two data.frames.
#' The joins will be done by all matching columns (identical names in both data.frames).
#'
#' @param LHS data.frame
#' @param RHS data.frame
#'
#' @return None, results will be printed
#'
#' @examples
#' SR_join_check(data.frame(a = 1:3),
#'               data.frame(a = c(3, 3:5)))
#'
#' @export
SR_join_check <- function(LHS = NULL, RHS = NULL) {
  # exit if error
  if (is.null(LHS) | is.null(RHS)) return(invisible(NULL))
  #
  # # identical column names
  col <- names(LHS)[names(LHS) %in% names(RHS)]
  #
  # show result of joins
  if (length(col) == 0) cat("No identical column names found :(")
  for (i in col) {
    cat(i, ":\n")
    cat("No. rows: LHS       : ", nrow(LHS))
    cat("  ( unique: ", nrow(LHS %>%
                               dplyr::ungroup() %>%
                               dplyr::select(dplyr::one_of(i)) %>%
                               dplyr::distinct()), ")\n")
    cat("No. rows: RHS       : ", nrow(RHS))
    cat("  ( unique: ", nrow(RHS %>%
                               dplyr::ungroup() %>%
                               dplyr::select(dplyr::one_of(i)) %>%
                               dplyr::distinct()), ")\n")
    cat("No. rows: left_join : ",
        LHS %>%
          dplyr::ungroup() %>%
          dplyr::select(dplyr::one_of(i)) %>%
          dplyr::left_join(RHS %>%
                             dplyr::ungroup() %>%
                             dplyr::select(dplyr::one_of(i)), by = i) %>%
          # dplyr::left_join(RHS %>% dplyr::select(dplyr::one_of(i)), by = i) %>%
          nrow(),
        "\n")
    cat("No. rows: right_join: ",
        LHS %>%
          dplyr::ungroup() %>%
          dplyr::select(dplyr::one_of(i)) %>%
          dplyr::right_join(RHS %>%
                              dplyr::ungroup() %>%
                              dplyr::select(dplyr::one_of(i)), by = i) %>%
          # dplyr::left_join(RHS %>% dplyr::select(dplyr::one_of(i)), by = i) %>%
          nrow(),
        "\n")
    cat("No. rows: inner_join: ",
        LHS %>%
          dplyr::ungroup() %>%
          dplyr::select(dplyr::one_of(i)) %>%
          dplyr::inner_join(RHS %>%
                              dplyr::ungroup() %>%
                              dplyr::select(dplyr::one_of(i)), by = i) %>%
          nrow(),
        "\n")
    cat("No. rows: full_join : ",
        LHS %>%
          dplyr::ungroup() %>%
          dplyr::select(dplyr::one_of(i)) %>%
          dplyr::full_join(RHS %>%
                             dplyr::ungroup() %>%
                             dplyr::select(dplyr::one_of(i)), by = i) %>%
          nrow(),
        "\n")
    cat("No. rows: anti_join : ",
        LHS %>%
          dplyr::ungroup() %>%
          dplyr::select(dplyr::one_of(i)) %>%
          dplyr::anti_join(RHS %>%
                             dplyr::ungroup() %>%
                             dplyr::select(dplyr::one_of(i)), by = i) %>%
          nrow(),
        "\n")
  }
}
