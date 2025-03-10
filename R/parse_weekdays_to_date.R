#' @title
#' Converts a dataframe row from a string containing a weekday to the date of the next occurance of that weekday.
#' 
#' @description
#' Function is designed to be used for the E-6 row. It converts strings suchas 'Scans sent Friday' to the date of the next occuring Friday.
#' 
#' 
#' @param process_row_df A dataframe with a single row and the columns: `Service`, `Develop Only`, `Dev + Scan`, `Prints Add On`. This corresponds to the turnaround time table from The Black and White Box website. This expects the E-6 row.
#' @param date_today A single Date. The date from which to determine the next occurance of a specific weekday.
#'
#' @returns A dataframe. The same as the input dataframe, but with the cells replaced by dates representing the turnaround date of the various services.
#' @export
#'
#' @examples
#' # Setup mock date and test calendar. These are required by the function.
#' today <- lubridate::ymd("2025-03-10")
#' test_cal <- bizdays::create.calendar(
#'   "test_cal",
#'   weekdays = c("saturday", "sunday")
#' )
#' 
#' # Example of an input dataframe
#' e6_string_df <- tibble::tibble(
#'   "Service" = "E-6",
#'   "Develop Only" = "Processed Wednesday",
#'   "Dev + Scan" = "Scans Sent Friday",
#'   "Prints Add On" = "+ 1 Working Days"
#' )
#' 
#' parse_weekdays_to_date(
#'   e6_string_df,
#'   biz_calendar_name = test_cal,
#'   date_today = today
#' )
#' # # A tibble: 1 × 4
#' #   Service `Develop Only` `Dev + Scan` `Prints Add On`
#' #   <chr>   <date>         <date>       <date>         
#' # 1 E-6     2025-03-13     2025-03-14   2025-03-17     
#' 
parse_weekdays_to_date <- function(
  process_row_df, 
  biz_calendar_name = "bawb_calendar",
  date_today = lubridate::today(tzone = "Pacific/Auckland")
) {

  dev_only_weekday <- extract_weekday_from_string(process_row_df$`Develop Only`)
  dev_and_scan_weekday <- extract_weekday_from_string(process_row_df$`Dev + Scan`)

  # E-6 dev only on turnaround times website says 'processed wednesday', but actually due the next day
  dev_only_days <- days_until_weekday(dev_only_weekday, date_from = date_today) + 1

  # For dev + scan, if the order comes in on wednesday, thursday or friday it won't be due until NEXT friday
  # This section handles if the order comes in on wednesday or thursday.
  # Since days_until_weekday() already excludes the current day, Friday handled implicitly
  today_wday <- stringr::str_to_lower(weekdays(date_today))
  if (today_wday %in% c("wednesday", "thursday")) {
    # Adding 3 days to the date_from parameter to get us past friday, and into next week.
    # This gets us days until friday from saturday or sunday. Then add the extra 3 days so it gets us
    # days until friday next week from wednesday/thursday this week.
    dev_and_scan_days <- 3 + days_until_weekday(dev_and_scan_weekday, date_from = lubridate::today(tzone = "Pacific/Auckland") + 3)
  } else {
    # All other days can be handled like normal
    dev_and_scan_days <- days_until_weekday(dev_and_scan_weekday, date_from = date_today)
  }

  dev_only_date <- date_today + dev_only_days
  dev_and_scan_date <- date_today + dev_and_scan_days
  prints_add_on_date <- bizdays::offset(dev_and_scan_date, 1, biz_calendar_name) # 1 extra business day

  result_df <- tibble::tibble(
    "Service" = process_row_df$Service,
    "Develop Only" = dev_only_date,
    "Dev + Scan" = dev_and_scan_date,
    "Prints Add On" = prints_add_on_date
  )

  return(result_df)
}
