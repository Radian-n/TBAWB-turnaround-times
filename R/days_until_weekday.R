days_until_weekday <- function(weekday, date_from = lubridate::today(tzone = "Pacific/Auckland")) {

  # Get today's weekday name
  today_weekday <- weekdays(date_from)

  # Get the next 7 weekdays from today.
  next_7_weekdays <- weekdays(c(1:7) + date_from)
  
  # Get the number of weekdays from today until `weekday_string`
  days_to_weekday_string <- match(weekday, next_7_weekdays)

  return(days_to_weekday_string)

} 