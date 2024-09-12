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
    # dplyr::mutate(years_ = ifelse(negative, ceiling(years), floor(years))) %>%
    # dplyr::mutate(years = lubridate::year(end_) -
    #          lubridate::year(start_) -
    #          pmax(lubridate::month(end_) < lubridate::month(start_),
    #               lubridate::month(end_) == lubridate::month(start_) &
    #                 lubridate::mday(end_) < lubridate::mday(start_)))
    # dplyr::mutate(years = lubridate::year(start_) - lubridate::year(end_))
    # years <- lubridate::time_length(lubridate::interval(start_, end_), "years")
    # years <- ifelse(negative_sign, ceiling(years), floor(years))
    # calculate fraction of year
    dplyr::mutate(start_is_leap =
                    ifelse(lubridate::year(start_) %% 400 == 0,
                           TRUE,
                           ifelse(lubridate::year(start_) %% 100 == 0,
                                  FALSE,
                                  ifelse(lubridate::year(start_) %% 4 == 0, TRUE, FALSE))),
                  end_is_leap =
                    ifelse(lubridate::year(end_) %% 400 == 0, TRUE,
                           ifelse(lubridate::year(end_) %% 100 == 0,
                                  FALSE,
                                  ifelse(lubridate::year(end_) %% 4 == 0, TRUE, FALSE))),
                  start_length = ifelse(start_is_leap, 366, 365),
                  end_length = ifelse(end_is_leap, 366, 365),
                  start_day = ifelse(start_is_leap & lubridate::yday(start_) >= 60,
                                     lubridate::yday(start_) - 1,
                                     lubridate::yday(start_)),
                  end_day = ifelse(end_is_leap & lubridate::yday(end_) >= 60,
                                   lubridate::yday(end_) - 1,
                                   lubridate::yday(end_)),
                  year_frac = ifelse(start_day < end_day,
                                     (end_day - start_day) / end_length,
                                     ifelse(start_day > end_day,
                                            (start_length - start_day) / start_length +
                                              end_day / end_length,
                                            0.0))) %>%
    # combine results
    dplyr::mutate(years_diff = ifelse(negative,
                                      -(years_ + year_frac), years_ + year_frac))
  #
  return(df$years_diff)
}

