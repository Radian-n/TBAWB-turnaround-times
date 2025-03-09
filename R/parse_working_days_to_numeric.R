#' Converts a dataframe with strings containing 'working days' into numeric values.
#' 
#' This function is designed for the following rows: C-41, B&W, ECN-2, ALT. It is NOT designed for E-6. The function for E-6 is \link[TBAWB-TURNAROUND-TIMES]{parse_weekdays_to_date()}.
#'
#' @param process_row_df A dataframe with a single row and the columns: `Service`, `Develop Only`, `Dev + Scan`, `Prints Add On`. This corresponds to the turnaround time table from The Black and White Box website.
#'
#' @returns A dataframe. The same as the input dataframe, but with the cells replaced by digits representing turnaround time in days.
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
#' parse_working_days_to_numeric(c41_string)
#' # A tibble: 1 × 4
#' #   Service `Develop Only` `Dev + Scan` `Prints Add On`
#' #   <chr>            <dbl>        <dbl>           <dbl>
#' # 1 C-41                 2            5               6
#' 
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
