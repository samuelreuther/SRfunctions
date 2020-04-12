#' Replace special characters in text
#'
#' Replaces the most common special characters.
#'
#' @param df data.frame
#' @param cols character (default = TRUE): select columns explicitly
#'
#' @return data.frame
#'
#' @examples
#' df <- data.frame(var_numeric = c(1, 2, 2, 3, NA, 4),
#'                  var_character = c("a", NA, "b", "c", "c", "d"),
#'                  var_factor = factor(c("a", "b", "c", "c", "d", NA)),
#'                  var_date = seq.Date(as.Date("2020-04-12"), by = "day", length.out = 6),
#'                  stringsAsFactors = FALSE)
#' SR_replace_umlaute(df)
#' rm(df)
#'
#' @export
SR_replace_umlaute <- function(df,
                               cols = "") {
  if (length(cols) == 1 & nchar(cols[1]) == 0) cols <- names(df)
  for (k in cols) {
    fc <- class(df[, k])[1] == "factor"
    if (fc) {
      df[, k] <- as.character(df[, k])
    } else if (class(df[, k])[1] != "character") {
      next()
    }
    #
    # convert from locale encoding to UTF-8
    # df[, k] <- iconv(df[, k], "", "UTF-8")
    #
    df[, k] <- gsub("ô", "o", df[, k], fixed = TRUE)
    df[, k] <- gsub("é", "e", df[, k], fixed = TRUE)
    df[, k] <- gsub("è", "e", df[, k], fixed = TRUE)
    #
    df[, k] <- gsub("Ã¤", "ae", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã'", "Ae", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ãf_", "Ae", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã¶", "oe", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã-", "Oe", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã_", "Oe", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã¼", "ue", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ão", "Ue", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã©", "e", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã¨", "e", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ãª", "e", df[, k], fixed = TRUE)
    df[, k] <- gsub("ÃY", "ss", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã´", "o", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã²", "o", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã³", "o", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ãª", "e", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã©", "e", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã¨", "e", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã¢", "a", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã¡", "a", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ã ", "a", df[, k], fixed = TRUE)
    #
    df[, k] <- gsub("\u00e4", "ae", df[, k], fixed = TRUE)
    df[, k] <- gsub("\u00c4", "Ae", df[, k], fixed = TRUE)
    df[, k] <- gsub("\u00f6", "oe", df[, k], fixed = TRUE)
    df[, k] <- gsub("\u00d6", "Oe", df[, k], fixed = TRUE)
    df[, k] <- gsub("\u00fc", "ue", df[, k], fixed = TRUE)
    df[, k] <- gsub("\u00dc", "Ue", df[, k], fixed = TRUE)
    df[, k] <- gsub("\u00df", "ss", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ae\u009c", "Ue", df[, k], fixed = TRUE)
    df[, k] <- gsub("Ae\u009f", "ss", df[, k], fixed = TRUE)
    #
    df[, k] <- gsub("[[:punct:]]", "_", df[, k])
    df[, k] <- gsub("[^[:alnum:]]", "_", df[, k])
    #
    df[, k] <- gsub("__", "_", df[, k])
    df[, k] <- gsub("__", "_", df[, k])
    df[, k] <- gsub("__", "_", df[, k])
    #
    df[, k] <- gsub("\\_$", "", df[, k])  # ends with "_"
    #
    if (fc) df[, k] <- as.factor(df[, k])
    rm(fc)
  }
  rm(k)
  return(df)
}
