parse_working_days_to_numeric <- function(process_row_df) {
  # For C-41, B&W, ECN-2, ALT
  # NOT E-6

  # Parse 'working days' strings into numbers  (e.g "3 working days")
  dev_only_days <- readr::parse_number(process_row_df$`Develop Only`)
  dev_and_scan_days <- readr::parse_number(process_row_df$`Dev + Scan`)

  # Parse additional days strings into numbers (e.g "+ 1 working day")
  prints_add_on_days <- dev_and_scan_days +
    readr::parse_number(process_row_df$`Prints Add On`)

  result_df <- tibble::tibble(
    "Service" = process_row_df$Service,
    "Develop Only" = dev_only_days,
    "Dev + Scan" = dev_and_scan_days,
    "Prints Add On" = prints_add_on_days
  )

  return(result_df)
}