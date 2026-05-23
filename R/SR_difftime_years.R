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

  n      <- max(length(startdate), length(enddate))
  result <- rep(NA_real_, n)

  # ok: scalar enddate/startdate recycled naturally by R's & operator
  ok <- !is.na(startdate) & !is.na(enddate)
  if (!any(ok)) return(result)

  # Keep scalar inputs as scalar (indexing a length-1 vector with a length-n logical
  # would pad with NAs for positions 2..n, introducing spurious NAs in 'negative')
  sd_ <- if (length(startdate) == 1L) startdate else startdate[ok]
  ed_ <- if (length(enddate)   == 1L) enddate   else enddate[ok]

  negative <- sd_ > ed_  # natural recycling when either is scalar

  s_int <- as.integer(sd_)  # stays length 1 when startdate is scalar
  e_int <- as.integer(ed_)  # stays length 1 when enddate is scalar

  # Swap: expand whichever scalar needs it — only when swaps actually occur
  m    <- max(length(s_int), length(e_int))  # = sum(ok) in the typical case
  swap <- which(negative)
  if (length(swap) > 0L) {
    if (length(s_int) < m) s_int <- rep_len(s_int, m)
    if (length(e_int) < m) e_int <- rep_len(e_int, m)
    tmp <- s_int[swap]; s_int[swap] <- e_int[swap]; e_int[swap] <- tmp
  }

  # Days-since-epoch → (y, m, d): Hinnant civil_from_days (pure integer arithmetic)
  # Replaces format(date, "%Y%m%d") + substr — no strftime calls
  .ymd <- function(z) {
    z   <- z + 719468L
    era <- z %/% 146097L
    doe <- z - era * 146097L
    yoe <- (doe - doe %/% 1460L + doe %/% 36524L - doe %/% 146096L) %/% 365L
    doy <- doe - (365L * yoe + yoe %/% 4L - yoe %/% 100L)
    mp  <- (5L * doy + 2L) %/% 153L
    d   <- doy - (153L * mp + 2L) %/% 5L + 1L
    m   <- mp + 3L - 12L * as.integer(mp >= 10L)
    y   <- yoe + era * 400L + as.integer(m <= 2L)
    list(y = y, m = m, d = d)
  }

  # (y, m, d) → days-since-epoch: proleptic Gregorian formula (pure integer arithmetic)
  # Replaces sprintf("%04d%02d%02d", ...) + as.Date(...) — no string allocation
  .dfe <- function(y, m, d) {
    yy <- y - as.integer(m <= 2L)
    mm <- m + 12L * as.integer(m <= 2L)
    365L * yy + yy %/% 4L - yy %/% 100L + yy %/% 400L +
      (153L * (mm - 3L) + 2L) %/% 5L + d - 719469L
  }

  s_ymd <- .ymd(s_int)
  sy <- s_ymd$y; sm <- s_ymd$m; sd <- s_ymd$d

  # scalar e_int → .ymd produces scalars for ey/em/ed; all arithmetic below recycles
  e_ymd <- .ymd(e_int)
  ey <- e_ymd$y; em <- e_ymd$m; ed <- e_ymd$d

  is_leap <- function(y) (y %% 400L == 0L) | ((y %% 4L == 0L) & (y %% 100L != 0L))

  # Whole years: replaces interval()/years(1) + floor()
  years_ <- ey - sy - as.integer((em < sm) | (em == sm & ed < sd))

  # Anniversary year components
  ann_yr <- sy + years_; ann_mo <- sm; ann_d <- sd

  # Feb 29 rollback: replaces add_with_rollback(years(years_))
  feb29        <- (ann_mo == 2L) & (ann_d == 29L) & !is_leap(ann_yr)
  ann_d[feb29] <- 28L

  # Anniversary date as integer — no sprintf/as.Date needed
  start__int   <- .dfe(ann_yr, ann_mo, ann_d)

  # Fractional year — same leap-year logic as original
  days_in_year <- 365L + as.integer(is_leap(ann_yr) | is_leap(ey))
  year_frac    <- (e_int - start__int) / days_in_year

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
