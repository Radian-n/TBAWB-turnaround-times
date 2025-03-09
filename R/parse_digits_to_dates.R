parse_digits_to_dates <- function(
  process_row_df,
  biz_calendar_name = "bawb_calendar",
  current_date = lubridate::floor_date(CURRENT_DATETIME, unit = "days")
) {
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