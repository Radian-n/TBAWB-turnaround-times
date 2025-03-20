#' @title
#' Gets the dates of summer closure for The Black and White Box for a given year
#'
#' @description
#' This isn't the period where The Black and White Box is closed, rather it is
#' the period where the turnaround times are paused until `january_end_day`.
#'
#' Note: The function returns the dates within *the same year*.
#' So to get a complete shutdown period, the function needs to be called
#' twice - once for the current year, and once for the next year (year + 1).
#'
#' @param year
#' A single numeric. The year to get the summer closure dates from.
#'
#' @param december_start_day
#' A single numeric. The day of Decmeber when the summer shutdown begins.
#'
#' @param january_end_day
#' A single numeric. The day of January when normal service resumes.
#'
#' @returns A vector of Dates. The summer shutdown period.
#' @export
#'
#' @examples
#' get_summer_holiday_dates(2024, december_start_day = 27, january_end_day = 5)
#' # [1] "2024-01-01" "2024-01-02" "2024-01-03" "2024-01-04" "2024-12-27"
#' # [6] "2024-12-28" "2024-12-29" "2024-12-30" "2024-12-31"
#'
get_summer_holiday_dates <- function(year, december_start_day = 28, january_end_day = 5) {

  bizdays::create.calendar(
    name = "weekend_cal",
    weekdays = c("saturday", "sunday"),
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  dec_holiday_start_date <- lubridate::dmy(glue::glue("{december_start_day}-12-{year}"))
  dec_holiday_end_date <- lubridate::dmy(glue::glue("31-12-{year}"))
  dec_holiday_dates_v <- seq.Date(dec_holiday_start_date, dec_holiday_end_date, by = "day")

  jan_holiday_start_date <- lubridate::dmy(glue::glue("1-1-{year}"))
  jan_holiday_end_date <- lubridate::dmy(glue::glue("{january_end_day}-1-{year}"))
  # Get first non-weekend day
  jan_holiday_end_date <- bizdays::following(jan_holiday_end_date, "weekend_cal")
  jan_holiday_end_date <- jan_holiday_end_date - 1 # Get day before first business day
  jan_holiday_end_date <- lubridate::ymd(jan_holiday_end_date) # Convert back to POSIXt
  jan_holiday_dates_v <- seq.Date(jan_holiday_start_date, jan_holiday_end_date, by = "day")

  holiday_dates_v <- c(dec_holiday_dates_v, jan_holiday_dates_v)
  holiday_dates_v <- sort(holiday_dates_v) # Return them in order

  return(holiday_dates_v)

}
