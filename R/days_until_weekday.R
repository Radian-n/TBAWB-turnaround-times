#' Calculate the number of days until a certain weekday.
#' 
#' The days from `date_from` until the next occurance of `weekday`.
#' 
#' If `date_from` is the same weekday as `weekday`, then function will return 7 days rather than 0. See examples.
#'
#' @param weekday A string. One of the weekdays, e.g "Monday".
#' @param date_from A date. The date from which to start calculating the number of days until `weekday`.
#'
#' @returns An integer. The number of days until `weekday`
#' @export
#'
#' @examples
#' monday <- lubridate::ymd("2025-03-10")
#' weekdays(monday)
#' # [1] "Monday"
#' 
#' days_until_weekday("Wednesday", date_from = monday)
#' # [1] 2
#' 
#' # Function exludes the current day. For example, 7 days from a wednesday until the next Wednesday, not 0 days.
#' wednesday <- lubridate::ymd("2025-03-12")
#' weekdays(wednesday)
#' # [1] "Wednesday"
#' days_until_weekday("Wednesday", date_from = wednesday)
#' # [1] 7
#' 
days_until_weekday <- function(weekday, date_from = lubridate::today(tzone = "Pacific/Auckland")) {

  # Check that `weekday` is a single string
  assertthat::assert_that(
    rlang::is_string(weekday),
    msg = glue::glue("`weekday` must be a single string.")
  )

  assertthat::assert_that(
    is.Date(date_from),
    msg = glue::glue("`date_from` must be of type date.")
  )

  assertthat::assert_that(
    length(date_from) == 1,
    msg = glue::glue("`date_from` must be of length one - A single date.")
  )

  # Ensure weekday sting starts with uppercase letter (e.g. "monday" => "Monday")
  weekday <- stringr::str_to_title(weekday)

  # Ensure a valid weekday is provided
  assertthat::assert_that(
    weekday %in% c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday"), 
    msg = glue::glue("`weekday` must be a day of the week, e.g 'Monday', not '{weekday}'.")
  )

  # Get today's weekday name
  today_weekday <- weekdays(date_from)

  # Get the next 7 weekdays from today.
  next_7_weekdays <- weekdays(c(1:7) + date_from)

  # Get the number of weekdays from today until `weekday_string`
  days_until_wday <- match(weekday, next_7_weekdays)

  return(days_until_wday)

}
