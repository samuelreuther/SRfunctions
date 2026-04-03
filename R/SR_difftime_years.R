#' Function to calculate correct years with floating points between two dates
#'
#' Sources:
#' - https://github.com/jknowles/eeptools/blob/main/R/age_calc.R
#' - https://stackoverflow.com/a/78022697/4786677
#'
#' @param enddate a vector of class Date representing the end date
#' @param startdate a vector of class Date representing the start date
#'
#' @return A numeric vector of ages the same length as the startdate vector
#'
#' @examples
#' SR_difftime_years(enddate = "2024-11-30", startdate = "2023-11-29")
#' SR_difftime_years(enddate = "2023-11-30", startdate = "2022-11-29")
#'
#' @export
SR_difftime_years <- function(enddate, startdate) {
  startdate <- as.Date(startdate)
  enddate   <- as.Date(enddate)

  # Recycle to common length (handles scalar enddate with vector startdate, and vice versa)
  n         <- max(length(startdate), length(enddate))
  startdate <- rep_len(startdate, n)
  enddate   <- rep_len(enddate, n)

  # Result initialised to NA_real_; only non-NA pairs are computed (avoids sprintf(NA) quirks)
  result <- rep(NA_real_, n)
  ok     <- !is.na(startdate) & !is.na(enddate)
  if (!any(ok)) return(result)

  sd_ <- startdate[ok]
  ed_ <- enddate[ok]

  negative <- sd_ > ed_

  # Swap for negative differences (integer representation, no copy of Date objects)
  s    <- as.integer(sd_)
  e    <- as.integer(ed_)
  swap <- which(negative)
  tmp  <- s[swap]; s[swap] <- e[swap]; e[swap] <- tmp
  start_ <- structure(s, class = "Date")
  end_   <- structure(e, class = "Date")

  # Year/month/day via format() — single C-level strftime call, avoids POSIXlt construction
  fmt_s <- format(start_, "%Y%m%d"); fmt_e <- format(end_, "%Y%m%d")
  sy <- as.integer(substr(fmt_s, 1L, 4L)); sm <- as.integer(substr(fmt_s, 5L, 6L)); sd <- as.integer(substr(fmt_s, 7L, 8L))
  ey <- as.integer(substr(fmt_e, 1L, 4L)); em <- as.integer(substr(fmt_e, 5L, 6L)); ed <- as.integer(substr(fmt_e, 7L, 8L))

  # Whole years: replaces interval()/years(1) + floor()
  years_ <- ey - sy - as.integer((em < sm) | (em == sm & ed < sd))

  # Anniversary year components
  ann_yr <- sy + years_
  ann_mo <- sm
  ann_d  <- sd

  # Vectorized leap year check: replaces the three-way ifelse chain
  is_leap <- function(y) (y %% 400L == 0L) | ((y %% 4L == 0L) & (y %% 100L != 0L))

  # Feb 29 rollback: replaces add_with_rollback(years(years_))
  feb29        <- (ann_mo == 2L) & (ann_d == 29L) & !is_leap(ann_yr)
  ann_d[feb29] <- 28L

  # Build anniversary Date via sprintf — single allocation, ISO format (fast as.Date path)
  start__      <- as.Date(sprintf("%04d%02d%02d", ann_yr, ann_mo, ann_d), format = "%Y%m%d")

  # Fractional year — same leap-year logic as original
  days_in_year <- 365L + as.integer(is_leap(ann_yr) | is_leap(ey))
  year_frac    <- (e - as.integer(start__)) / days_in_year

  r           <- years_ + year_frac
  r[negative] <- -r[negative]
  result[ok]  <- r
  result
}
#
# SR_difftime_years <- function(enddate, startdate){
#   startdate <- as.Date(startdate)
#   enddate <- as.Date(enddate)
#   if (!inherits(startdate, "Date") | !inherits(enddate, "Date")) {
#     stop("Both startdate and enddate must be Date class objects")
#   }
#   #
#   df <- data.frame(startdate,
#                    enddate,
#                    negative = startdate > enddate,
#                    start = as.POSIXlt(startdate),
#                    end = as.POSIXlt(enddate)) %>%
#     dplyr::mutate(start_ = dplyr::if_else(negative, end, start),
#                   end_ = dplyr::if_else(negative, start, end)) %>%
#     # calculate years
#     dplyr::mutate(years = lubridate::interval(start_, end_) / lubridate::years(1)) %>%
#     dplyr::mutate(years_ = floor(years)) %>%
#     dplyr::mutate(start__ = start_ %>%
#                     lubridate::add_with_rollback(lubridate::years(years_)),
#                   start_is_leap =
#                     ifelse(lubridate::year(start__) %% 400 == 0,
#                            TRUE,
#                            ifelse(lubridate::year(start__) %% 100 == 0,
#                                   FALSE,
#                                   ifelse(lubridate::year(start__) %% 4 == 0, TRUE, FALSE))),
#                   end_is_leap =
#                     ifelse(lubridate::year(end_) %% 400 == 0, TRUE,
#                            ifelse(lubridate::year(end_) %% 100 == 0,
#                                   FALSE,
#                                   ifelse(lubridate::year(end_) %% 4 == 0, TRUE, FALSE))),
#                   length = ifelse(start_is_leap | end_is_leap, 366, 365),
#                   diff_days = as.numeric(difftime(end_, start__, units = "days")),
#                   year_frac = diff_days / length) %>%
#     # combine results
#     dplyr::mutate(years_diff = ifelse(negative,
#                                       -(years_ + year_frac), years_ + year_frac))
#   #
#   return(df$years_diff)
# }
