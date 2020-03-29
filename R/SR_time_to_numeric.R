#' Calculate numeric value of a timestamp
#'
#' calculate numeric value of a timestamp from an object in format "HH:MM:SS"
#'
#' @param x object or variable in format "HH:MM:SS"
#'
#' @return numeric object or variable
#'
#' @examples
#' SR_time_to_numeric("12:30:05")
#'
#' @export
SR_time_to_numeric <- function(x) {
  # change character to POSIXct
  temp <- strptime(x, "%H:%M:%S")
  #
  # calculate numeric time
  temp <- as.numeric(format(temp, "%H")) +
    as.numeric(format(temp, "%M")) / 60 +
    as.numeric(format(temp, "%S")) / 60 / 60
  #
  # return time as numeric
  return(temp)
}
