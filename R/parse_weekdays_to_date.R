#' @title
#' Converts a dataframe row from a string containing a weekday to the date of the next occurance of that weekday.
#'
#' @description
#' Function is designed to be used for the E-6 row.
#' It converts strings suchas 'Scans sent Friday' to the date of the next occuring Friday.
#'
#'
#' @param process_row_df A dataframe with a single row and the columns:
#' `Service`, `Develop Only`, `Dev + Scan`, `Prints Add On`.
#' This corresponds to the turnaround time table from The Black and White Box website. This expects the E-6 row.
#' @param date_today A single Date. The date from which to determine the next occurance of a specific weekday.
#'
#' @returns A dataframe.
#' The same as the input dataframe, but with the cells replaced by dates representing the turnaround date of services.
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
#'   date_today = today,
#'   biz_calendar_name = test_cal   
#' )
#' # # A tibble: 1 × 4
#' #   Service `Develop Only` `Dev + Scan` `Prints Add On`
#' #   <chr>   <date>         <date>       <date>
#' # 1 E-6     2025-03-13     2025-03-14   2025-03-17
#'
parse_weekdays_to_date <- function(
  process_row_df,
  date_today,
  biz_calendar_name = "bawb_calendar"
) {
  # Argument: process_row_df
  assertthat::assert_that(
    is.data.frame(process_row_df),
    msg = "`process_row_df` must be a dataframe"
  )
  assertthat::assert_that(
    all(dim(process_row_df) == c(1, 4)),
    msg = "`process_row_df` must have 1 row and 4 columns"
  )
  assertthat::assert_that(
    all(
      colnames(process_row_df) %in%
        c("Service", "Develop Only", "Dev + Scan", "Prints Add On")
    ),
    msg = glue::glue(
      "`process_row_df` must have column names: 'Service', 'Develop Only', 'Dev + Scan', 'Prints Add On'."
    )
  )
  assertthat::assert_that(
    "E-6" %in% process_row_df$Service,
    msg = glue::glue(
      "This function should only be used to handle E-6 turnaround times, not C-41, B&W, etc."
    )
  )

  # Argument: biz_calendar_name
  assertthat::assert_that(
    rlang::is_string(biz_calendar_name),
    msg = glue::glue(
      "`biz_calendar_name` must be a string - the name of an existing bizdays calendar."
    )
  )
  assertthat::assert_that(
    biz_calendar_name %in% names(bizdays::calendars()),
    msg = glue::glue(
      "`biz_calendar_name` must be a valid bizdays calendar. 
      Use bizdays::calendars() to check existing calendars, and bizdays::create.calendar() to create a one."
    )
  )

  # Argument: date_today
  assertthat::assert_that(
    lubridate::is.Date(date_today),
    msg = glue::glue(
      "`date_today` must be of type Date, not type {typeof(date_today)}."
    )
  )
  assertthat::assert_that(
    length(date_today) == 1,
    msg = glue::glue(
      "`date_today` must be a single date, not a vector of length: {length(date_today)}."
    )
  )

  dev_only_weekday <- extract_weekday_from_string(process_row_df$`Develop Only`)
  dev_and_scan_weekday <- extract_weekday_from_string(
    process_row_df$`Dev + Scan`
  )

  # E-6 dev only on turnaround times website says 'processed wednesday', but actually due the next day
  dev_only_days <- days_until_weekday(
    dev_only_weekday,
    date_from = date_today
  ) +
    1

  # For dev + scan, if the order comes in on wednesday, thursday or friday it won't be due until NEXT friday
  # This section handles if the order comes in on wednesday or thursday.
  # Since days_until_weekday() already excludes the current day, Friday handled implicitly
  today_wday <- stringr::str_to_lower(weekdays(date_today))
  if (today_wday %in% c("wednesday", "thursday")) {
    # Adding 3 days to the date_from parameter to get us past friday, and into next week.
    # This gets us days until friday from saturday or sunday. Then add the extra 3 days so it gets us
    # days until friday next week from wednesday/thursday this week.
    dev_and_scan_days <- 3 +
      days_until_weekday(
        dev_and_scan_weekday,
        date_from = date_today + 3
      )
  } else {
    # All other days can be handled like normal
    dev_and_scan_days <- days_until_weekday(
      dev_and_scan_weekday,
      date_from = date_today
    )
  }

  # dev_only_date is always a thursday
  thursday <- date_today + dev_only_days
  dev_only_date <- date_today + dev_only_days
  dev_and_scan_date <- date_today + dev_and_scan_days

  # Returns the next business day if Wednesday is a public holiday
  if (!bizdays::is.bizday(thursday - 1, biz_calendar_name)) {
    dev_only_date <- bizdays::offset(dev_only_date, 1, biz_calendar_name)
    dev_and_scan_date <- bizdays::offset(dev_and_scan_date, 1, biz_calendar_name)
  }
  # Returns the next business day if Thursday is a public holiday
  if (!bizdays::is.bizday(thursday, biz_calendar_name)) {
    dev_only_date <- bizdays::offset(dev_only_date, 1, biz_calendar_name)
    dev_and_scan_date <- bizdays::offset(dev_and_scan_date, 1, biz_calendar_name)
  }
  # Returns the next business day if Friday is a public holiday
  if (!bizdays::is.bizday(thursday + 1, biz_calendar_name)) {
    # No dev_only_date because it is completed on Thursday
    dev_and_scan_date <- bizdays::offset(dev_and_scan_date, 1, biz_calendar_name)
  }

  prints_add_on_date <- bizdays::offset(dev_and_scan_date, 1, biz_calendar_name) # 1 extra business day

  result_df <- tibble::tibble(
    "Service" = process_row_df$Service,
    "Develop Only" = dev_only_date,
    "Dev + Scan" = dev_and_scan_date,
    "Prints Add On" = prints_add_on_date
  )

  return(result_df)
}
