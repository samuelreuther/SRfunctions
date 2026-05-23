#' Clean sandbox directory
#'
#' Removes common R/Python build artifacts from a directory and its
#' subdirectories: \code{Rplots.pdf}, \code{hs_err_pid*.log}, and
#' \code{.ipynb_checkpoints} folders. Writes a timestamp to
#' \code{<path>/Functions/.last_cleanup.rds} after a successful run.
#'
#' @param path character: path to the sandbox root directory
#'
#' @return invisible character vector of removed paths
#'
#' @examples
#' \dontrun{
#' SR_clean_sandbox(path = "/home/sandbox/sandbox")
#' }
#'
#' @export
SR_clean_sandbox <- function(path = "/home/sandbox/sandbox") {
  if (!dir.exists(path)) return(invisible())
  #
  all_files <- fs::dir_ls(path, recurse = TRUE, all = TRUE)
  #
  # Rplots.pdf, hs_err_pid*.log, .ipynb_checkpoints folders
  targets <- all_files[grepl("Rplots\\.pdf$|hs_err_pid.*\\.log$|\\.ipynb_checkpoints$",
                             all_files, useBytes = TRUE)]
  #
  if (length(targets) > 0) {
    unlink(targets, recursive = TRUE)
    message(sprintf("Removed %d artifact(s).", length(targets)))
  }
  saveRDS(Sys.time(), "/home/sandbox/sandbox/Functions/.last_cleanup.rds") # timestamp, hardcoded path
  invisible(targets)
}
