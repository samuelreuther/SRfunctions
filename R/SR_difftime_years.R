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
SR_difftime_years <- function(enddate,
                              startdate){
  startdate <- as.Date(startdate)
  enddate <- as.Date(enddate)
  if (!inherits(startdate, "Date") | !inherits(enddate, "Date")) {
    stop("Both startdate and enddate must be Date class objects")
  }
  #
  df <- data.frame(startdate,
                   enddate,
                   negative = startdate > enddate,
                   start = as.POSIXlt(startdate),
                   end = as.POSIXlt(enddate)) %>%
    dplyr::mutate(start_ = dplyr::if_else(negative, end, start),
                  end_ = dplyr::if_else(negative, start, end)) %>%
    # calculate years
    dplyr::mutate(years = lubridate::interval(start_, end_) / lubridate::years(1)) %>%
    dplyr::mutate(years_ = floor(years)) %>%
    dplyr::mutate(start__ = start_ + lubridate::years(years_),
                  start_is_leap =
                    ifelse(lubridate::year(start__) %% 400 == 0,
                           TRUE,
                           ifelse(lubridate::year(start__) %% 100 == 0,
                                  FALSE,
                                  ifelse(lubridate::year(start__) %% 4 == 0, TRUE, FALSE))),
                  end_is_leap =
                    ifelse(lubridate::year(end_) %% 400 == 0, TRUE,
                           ifelse(lubridate::year(end_) %% 100 == 0,
                                  FALSE,
                                  ifelse(lubridate::year(end_) %% 4 == 0, TRUE, FALSE))),
                  length = ifelse(start_is_leap | end_is_leap, 366, 365),
                  diff_days = as.numeric(difftime(end_, start__, units = "days")),
                  year_frac = diff_days / length) %>%
    # combine results
    dplyr::mutate(years_diff = ifelse(negative,
                                      -(years_ + year_frac), years_ + year_frac))
  #
  return(df$years_diff)
}
