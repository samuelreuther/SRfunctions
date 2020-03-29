SR_join_check <- function(LHS = NULL, RHS = NULL) {
  # exit if error
  if (is.null(LHS) | is.null(RHS)) break()
  #
  # # identical column names
  col <- names(LHS)[names(LHS) %in% names(RHS)]
  #
  # show result of joins
  if (length(col) == 0) cat("No identical column names found :(")
  for (i in col) {
    cat(i, ":\n")
    cat("No. rows: LHS       : ", nrow(LHS))
    cat("  ( unique: ", nrow(LHS %>% ungroup() %>% select(one_of(i)) %>% distinct()), ")\n")
    cat("No. rows: RHS       : ", nrow(RHS))
    cat("  ( unique: ", nrow(RHS %>% ungroup() %>% select(one_of(i)) %>% distinct()), ")\n")
    cat("No. rows: left_join : ",
        LHS %>% ungroup() %>% select(one_of(i)) %>%
          left_join(RHS %>% ungroup() %>% select(one_of(i)), by = i) %>%
          # left_join(RHS %>% select(one_of(i)), by = i) %>%
          nrow(),
        "\n")
    cat("No. rows: right_join: ",
        LHS %>% ungroup() %>% select(one_of(i)) %>%
          right_join(RHS %>% ungroup() %>% select(one_of(i)), by = i) %>%
          # left_join(RHS %>% select(one_of(i)), by = i) %>%
          nrow(),
        "\n")
    cat("No. rows: inner_join: ",
        LHS %>% ungroup() %>% select(one_of(i)) %>%
          inner_join(RHS %>% ungroup() %>% select(one_of(i)), by = i) %>%
          nrow(),
        "\n")
    cat("No. rows: full_join : ",
        LHS %>% ungroup() %>% select(one_of(i)) %>%
          full_join(RHS %>% ungroup() %>% select(one_of(i)), by = i) %>%
          nrow(),
        "\n")
    cat("No. rows: anti_join : ",
        LHS %>% ungroup() %>% select(one_of(i)) %>%
          anti_join(RHS %>% ungroup() %>% select(one_of(i)), by = i) %>%
          nrow(),
        "\n")
  }
}
