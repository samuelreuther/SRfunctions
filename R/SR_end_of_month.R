SR_end_of_month <- function(date) {
  # add a month, then subtract a day
  date.lt <- as.POSIXlt(date)
  mon <- date.lt$mon + 2
  year <- date.lt$year
  year <- year + as.integer(mon == 13) # if month was December add a year
  mon[mon == 13] <- 1
  iso <- ISOdate(1900 + year, mon, 1, hour = 0) # , tz=attr(date,"tz")
  result <- as.POSIXct(iso) - 86400 # subtract one day
  result <- result + (as.POSIXlt(iso)$isdst - as.POSIXlt(result)$isdst) * 3600
  return(result)
}
