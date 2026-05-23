#' Update .gitignore files
#'
#' Propagates entries from a source .gitignore to all .gitignore files found
#' in a target directory and its subdirectories. Only entries not already
#' present in a target file are appended (diff-based update).
#'
#' @param from character: path to the source .gitignore file
#' @param to character: path to the root directory to search for .gitignore files
#'
#' @return invisible(NULL), results are printed
#'
#' @examples
#' \dontrun{
#' SR_update_gitignore(from = "/home/sandbox/sandbox/Functions/.gitignore",
#'                     to = "/home/sandbox/sandbox/")
#' }
#'
#' @export
SR_update_gitignore <- function(from = NULL, to = NULL) {
  # exit if error
  if (is.null(from) | is.null(to)) return(invisible(NULL))
  if (!file.exists(from)) {
    cat("Source file not found:", from, "\n")
    return(invisible(NULL))
  }
  if (!dir.exists(to)) {
    cat("Target directory not found:", to, "\n")
    return(invisible(NULL))
  }
  #
  # read source .gitignore — keep only active pattern lines for comparison
  source_lines <- readLines(from, warn = FALSE)
  source_patterns <- trimws(source_lines[nchar(trimws(source_lines)) > 0 &
                                           !grepl("^\\s*#", source_lines)])
  #
  # find all .gitignore files recursively in target directory, excluding source
  gitignore_files <- list.files(path = to, pattern = "^\\.gitignore$",
                                full.names = TRUE, recursive = TRUE,
                                all.files = TRUE)
  gitignore_files <- gitignore_files[normalizePath(gitignore_files) !=
                                       normalizePath(from)]
  #
  if (length(gitignore_files) == 0) {
    cat("No .gitignore files found in:", to, "\n")
    return(invisible(NULL))
  }
  #
  cat("Found", length(gitignore_files), ".gitignore file(s)\n\n")
  #
  # update each .gitignore
  for (f in gitignore_files) {
    target_lines <- readLines(f, warn = FALSE)
    target_patterns <- trimws(target_lines[nchar(trimws(target_lines)) > 0 &
                                             !grepl("^\\s*#", target_lines)])
    #
    # patterns in source not yet present in target
    new_patterns <- source_patterns[!source_patterns %in% target_patterns]
    #
    cat(f, ":\n")
    if (length(new_patterns) == 0) {
      cat("  -> already up to date\n\n")
    } else if (!file.access(f, mode = 2) == 0) {
      cat("  -> skipped (permission denied)\n\n")
    } else {
      cat("  -> appending", length(new_patterns), "new pattern(s):",
          paste(new_patterns, collapse = ", "), "\n\n")
      write(c("",
              paste0("# added by SR_update_gitignore (", Sys.Date(), ")"),
              new_patterns),
            file = f, append = TRUE)
    }
  }; rm(f)
  #
  invisible(NULL)
}
