#' Calculate date at end of month
#'
#' by adding a month, then subtract a day
#'
#' @param x date
#'
#' @return date
#'
#' @examples
#' SR_end_of_month("2019-12-24")
#'
#' @export
SR_end_of_month <- function(date) {
  #
  date.lt <- as.POSIXlt(date)
  mon <- date.lt$mon + 2
  year <- date.lt$year
  year <- year + as.integer(mon == 13) # if month was December add a year
  mon[mon == 13] <- 1
  iso <- ISOdate(1900 + year, mon, 1, hour = 0, tz = "GMT")
  result <- as.POSIXct(iso) - 86400 # subtract one day
  result <- result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst) * 3600
  return(as.Date(result))
}
