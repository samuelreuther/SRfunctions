test_that("1 year + 1 day (leap year)", {
  expect_equal(round(SR_difftime_years(enddate = "2024-11-30", startdate = "2023-11-29"), 6),
               1.002732)
})

test_that("1 year + 1 day (no leap year)", {
  expect_equal(round(SR_difftime_years(enddate = "2023-11-30", startdate = "2022-11-29"), 6),
               1.002740)
})

test_that("NA", {
  expect_equal(SR_difftime_years(enddate = "2023-11-30", startdate = NA),
               NA)
})

test_that("several dates", {
  expect_equal(round(SR_difftime_years(enddate = "2023-11-30",
                                       startdate = c("2023-11-29", "2023-11-30", "2023-12-01",
                                                     "2024-11-29", "2024-11-30", "2024-12-01")), 6),
               c(0.002740, 0.000000, -0.002740, -0.997268, -1.000000, -1.002732))
})

# test_that("df", {
#   expect_equal({
#     ADWHutilities::Load_data(table_name = "gp_status_hist",
#                              limit = 200, random = TRUE) %>%
#       dplyr::mutate(VERTRAGAB_JAHRE =
#                       round(SR_difftime_years(
#                         enddate = VERTRAGAB, startdate = "2023-11-30"), 3)) %>%
#       class()
#   }, "data.frame")
# })

# test_that("df", {
#   expect_equal({
#     library(dplyr)
#     data.frame(stichtag = c(seq.Date(from = as.Date("2023-01-01"),
#                                      to = as.Date("2024-12-31"),
#                                      by = "month"),
#                             NA, lubridate::NA_Date_)) %>%
#       dplyr::mutate(difftime_years = SR_difftime_years(enddate = "2023-11-01",
#                                                        startdate = stichtag)) %>%
#       class()
#   }, "data.frame")
# })

# system.time({
#   x <-  ADWHutilities::Load_data(table_name = "gp_status_hist",
#                                  stichtag_data = "2023-11-30",
#                                  only_active_customers = TRUE,
#                                  # limit = 100000,
#                                  path_data = getwd())
# })
#
# system.time({
#   x <-  ADWHutilities::Load_data(table_name = "gp_status_hist",
#                                  stichtag_data = "2023-11-30",
#                                  only_active_customers = TRUE,
#                                  # limit = 100000,
#                                  path_data = getwd()) %>%
#     mutate(VERTRAGAB_JAHRE =
#              round(SR_difftime_years(
#                enddate = VERTRAGAB, startdate = "2023-11-30"), 3))
# })

# SR_difftime_years("2023-02-01", "2023-02-02")
# SR_difftime_years("2024-02-01", "2024-02-02")
# SR_difftime_years("2025-02-01", "2025-02-02")
#
# SR_difftime_years("2023-11-30", "2023-11-29")
# SR_difftime_years("2024-11-30", "2024-11-29")
# SR_difftime_years("2025-11-30", "2025-11-29")
#
# round(SR_difftime_years(enddate = "2023-11-30", startdate = "1992-12-01"), 4)

