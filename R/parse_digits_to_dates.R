#' Converts turnaround time dataframe row to due dates.
#' 
#' This function is designed for the following rows: C-41, B&W, ECN-2, ALT. It is NOT designed for E-6. This function is generally expected to be used directly after [parse_working_days_to_numeric()].
#' 
#' Uses the {bizdays} package with a custom calendar definition to calculate the due dates for various lab services from the provided working days.
#'
#' @param process_row_df A dataframe with a single row and the columns: `Service`, `Develop Only`, `Dev + Scan`, `Prints Add On`. This corresponds to the turnaround time table from The Black and White Box website. Usually the direct output of [parse_working_days_to_numeric()].
#' @param biz_calendar_name A string. The name of the bizdays calendar to use. Usually "bawb_calendar".
#' @param current_date A date. The starting point for the due date calculation.
#'
#' @returns A dataframe. The same as the input dataframe, but with the cells replaced by dates representing the due dates for the various services.
#' @export
#'
#' @examples
#' c41_string <- tibble::tibble(
#'   "Service" = "C-41",
#'   "Develop Only" = "2 Working Days",
#'   "Dev + Scan" = "5 Working Days",
#'   "Prints Add On" = "+ 1 Working Days"
#' )
#' 
#' c41_string
#' # # A tibble: 1 × 4
#' #   Service `Develop Only` `Dev + Scan`   `Prints Add On` 
#' #   <chr>   <chr>          <chr>          <chr>           
#' # 1 C-41    2 Working Days 5 Working Days + 1 Working Days
#' 
#' # Notice that `Prints Add On` result is `Dev + Scan` + 1
#' c41_working_days <- parse_working_days_to_numeric(c41_string)
#' c41_working_days
#' # A tibble: 1 × 4
#' #   Service `Develop Only` `Dev + Scan` `Prints Add On`
#' #   <chr>            <dbl>        <dbl>           <dbl>
#' # 1 C-41                 2            5               6
#' 
#' today <- lubridate::ymd("2025-03-10")
#' test_cal <- bizdays::create.calendar(
#'   "test_cal",
#'   weekdays = c("saturday", "sunday")
#' )
#' parse_digits_to_dates(
#'   c41_working_days, 
#'   biz_calendar_name = test_cal, 
#'   current_date = today
#' )
#' # # A tibble: 1 × 4
#' #   Service `Develop Only` `Dev + Scan` `Prints Add On`
#' #   <chr>   <date>         <date>       <date>         
#' # 1 C-41    2025-03-12     2025-03-17   2025-03-18     
#' 
#' 
parse_digits_to_dates <- function(
  process_row_df,
  biz_calendar_name = "bawb_calendar",
  current_date = lubridate::floor_date(CURRENT_DATETIME, unit = "days")
) {

  # Argument: process_row_df
  assertthat::assert_that(is.data.frame(process_row_df), msg = "`process_row_df` must be a dataframe")
  assertthat::assert_that(all(dim(process_row_df) == c(1, 4)), msg = "`process_row_df` must have 1 row and 4 columns")
  assertthat::assert_that(
    all(colnames(process_row_df) %in% c("Service", "Develop Only", "Dev + Scan", "Prints Add On")),
    msg = glue::glue("`process_row_df` must have column names: 'Service', 'Develop Only', 'Dev + Scan', 'Prints Add On'.")
  )
  assertthat::assert_that(
    !("E-6" %in% process_row_df$Service), 
    msg = "This function is not designed to handle E-6 turnaround times. These are static (e.g 'Processed Wednesday') and should be used with parse_weekdays_to_date()"
  )

  # Argument: biz_calendar_name
  assertthat::assert_that(
    biz_calendar_name %in% names(bizdays::calendars()),
    msg = glue::glue("`biz_calendar_name` must be a valid bizdays calendar. Use bizdays::calendars() to check existing calendars, and bizdays::create.calendar() to create a bizdays calendar.")
  )

  # Argument: current_date
  assertthat::assert_that(
    lubridate::is.Date(current_date), 
    msg = glue::glue("`current_date` must be of type Date, not type {typeof(current_date)}.")
  )
  assertthat::assert_that(
    length(current_date) == 1,
    msg = glue::glue("`current_date` must be a single date, not a vector of length: {length(current_date)}.")
  )

  # Calculate dev only due date
  dev_only_date <- bizdays::offset(
    current_date,
    process_row_df$`Develop Only`,
    biz_calendar_name
  )

  # Calculate dev and scan due date
  dev_and_scan_date <- bizdays::offset(
    current_date,
    process_row_df$`Dev + Scan`,
    biz_calendar_name
  )

  # Calculate print add on due date
  prints_add_on_date <- bizdays::offset(
    current_date,
    process_row_df$`Prints Add On`,
    biz_calendar_name
  )

  result_df <- tibble::tibble(
    "Service" = process_row_df$Service,
    "Develop Only" = dev_only_date,
    "Dev + Scan" = dev_and_scan_date,
    "Prints Add On" = prints_add_on_date
  )

  return(result_df)
}
