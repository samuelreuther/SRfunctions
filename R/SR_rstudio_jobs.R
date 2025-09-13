## List RStudio Jobs as data.frame
#'
#' Return a tibble of Jobs shown in RStudio's Jobs pane
#'
#' This is a minimal wrapper that fetches the job list and maps a few
#' common fields into columns. It requires running inside RStudio.
#'
#' @return tibble
#' @examples
#' \dontrun{
#'   SR_get_rstudio_jobs()
#' }
#' @export
SR_get_rstudio_jobs <- function() {
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop("RStudio API not available. Run inside RStudio.")
  }
  #
  jobs <- rstudioapi::jobList()
  if (!length(jobs)) return(tibble::tibble())
  #
  started_num <- sapply(jobs, function(x) x$started, USE.NAMES = FALSE)
  started_at  <- as.POSIXct(started_num, origin = "1970-01-01", tz = Sys.timezone())
  elapsed_min = round(sapply(jobs, function(x) x$elapsed, USE.NAMES = FALSE) / 60, 2)
  #
  tibble::tibble(
    id = sapply(jobs, function(x) x$id, USE.NAMES = FALSE),
    name = sapply(jobs, function(x) x$name, USE.NAMES = FALSE),
    state_description = sapply(jobs, function(x) x$state_description, USE.NAMES = FALSE),
    progress = sapply(jobs, function(x) x$progress, USE.NAMES = FALSE),
    started = started_at,
    elapsed = elapsed_min
  )
}

