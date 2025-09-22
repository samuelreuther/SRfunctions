#' Maskierte Vorschau eines Data Frames
#'
#' Druckt eine maskierte Vorschau der ersten n Zeilen.
#'
#' Maskierungsregeln:
#' - ID-/Nummernspalten (z. B. Zepas_ID, POLICEN_ID, POLICE_NR, Laufnummer) werden so maskiert,
#'   dass nur die erste und letzte Stelle sichtbar bleiben, der Rest wird mit "0" ersetzt.
#'   Beispiel: 123456 -> 1000006, "A-123456" -> "A-00006"
#' - Namensspalten (z. B. Name, Vorname, Nachname, Telefon, Mobile, Email, Fax) werden so
#'   maskiert, dass nur der erste Buchstabe sichtbar bleibt, der Rest wird durch
#'   "x" ersetzt. Beispiel: "Michael" -> "Mxxxxxx"
#'
#' @param df Ein data.frame oder tibble.
#' @param n Anzahl Zeilen für die Vorschau (Standard: 5).
#' @param id_patterns Optionale Regex-Vektoren für ID-/Nummernspalten
#' @param name_patterns Optionale Regex-Vektoren für Namensspalten
#' @return (Invisibly) das maskierte data.frame
#' @examples
#' df <- data.frame(GP_ID = c(123456, 987654), Name = c("Michael", "Anna"))
#' SR_view(df, n = 2)
#' @export
SR_view <- function(df, n = 5, verbose = TRUE,
                      id_patterns = c(
                        "(?i)(^|_)(kunde|kunden|zepas|gp|policen?|police|vertrag|laufnummer|id|nr)(_|$)",
                        "(?i)(id$|nr$)"),
                      name_patterns = c("(?i)(^|_)(vorname|nachname|name|strasse)(_|$)",
                                        "(?i)(^|_)(telefon|mobile|email|fax)(_|$)")) {
  stopifnot(is.data.frame(df))
  n <- max(1L, as.integer(n))
  cn <- colnames(df)
  #
  # Helper: test if column name matches any of the provided regexes
  matches_any <- function(x, patterns)
    any(vapply(patterns, function(p) grepl(p, x, perl = TRUE), logical(1)))
  #
  # Which columns are IDs or Names?
  id_cols   <- vapply(cn, matches_any, logical(1), patterns = id_patterns)
  name_cols <- vapply(cn, matches_any, logical(1), patterns = name_patterns)
  #
  # Masking helpers
  mask_number_digits <- function(v) {
    # Keep first & last digit, zero out middle; keep numeric type
    if (!length(v)) return(v)
    vx <- as.integer(round(v))
    res <- vx
    ok <- !is.na(vx)
    if (!any(ok)) return(res)
    absx <- abs(vx[ok])
    nd <- nchar(as.character(absx))
    do <- nd >= 3L # only if there are middle digits
    if (any(do)) {
      a <- absx[do]
      n <- nd[do]
      first <- a %/% 10^(n - 1L)
      last <- a %% 10L
      val <- first * 10^(n - 1L) + last
      res_ok <- vx[ok]
      res_ok[do] <- ifelse(res_ok[do] < 0L, -val, val)
      res[ok] <- res_ok
    }
    res
  }
  #
  mask_char_digits <- function(x) {
    # Apply to each \d+ block within the string; keep character type
    repl_block <- function(block) {
      L <- nchar(block)
      if (L <= 2L) return(block)
      paste0(substr(block, 1, 1), paste(rep("0", L - 2L), collapse = ""), substr(block, L, L))
    }
    mask_one <- function(s) {
      if (is.na(s)) return(s)
      m <- gregexpr("\\d+", s, perl = TRUE)
      blocks <- regmatches(s, m)[[1]]
      if (length(blocks) == 0L) return(s)
      blocks2 <- vapply(blocks, repl_block, character(1))
      regmatches(s, m) <- list(blocks2)
      s
    }
    vapply(as.character(x), mask_one, character(1))
  }
  #
  mask_name <- function(x) {
    to_char <- as.character(x)
    mask_one <- function(s) {
      if (is.na(s)) return(s)
      chars <- strsplit(s, "", fixed = TRUE)[[1]]
      L <- length(chars)
      if (L <= 1) return(s)
      paste0(chars[1], paste(rep("x", L - 1), collapse = ""))
    }
    vapply(to_char, mask_one, character(1))
  }
  #
  masked <- utils::head(df, n)
  for (j in seq_along(masked)) {
    col <- masked[[j]]
    if (id_cols[j]) {
      if (is.numeric(col)) {
        masked[[j]] <- mask_number_digits(col)
      } else if (is.character(col)) {
        masked[[j]] <- mask_char_digits(col)
      } else {
        # leave other types unchanged
        masked[[j]] <- col
      }
    } else if (name_cols[j]) {
      masked[[j]] <- mask_name(col)
    }
  }
  #
  # Print a compact preview
  if (verbose) print(masked)
  invisible(masked)
}

