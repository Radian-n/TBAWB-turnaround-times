#' Scrapes the turnaround time data from The Black and White Box website.
#'
#' @param get_table A single string.
#' The turnaround time table to scrape.
#' "film" to get the film-processing related turnaround times.
#' "print" to get the printing related turnaround times.
#'
#' @returns A dataframe. The turnaround times for the specified table.
#' @export
scrape_turnaround_times <- function(get_table = "film") {
  # Allow multiple attempts at reading the web page
  insistent_read_html <- purrr::insistently(
    f = rvest::read_html,
    rate = purrr::rate_backoff(
      pause_base = 2,
      pause_cap = 120,
      pause_min = 1,
      max_times = 5,
      jitter = TRUE
    )
  )

  # Scrape turnaround times page
  turnaround_times <- insistent_read_html("https://theblackandwhitebox.co.nz/turn-around-times/")

  # Extract turnaround times tables (film lab, printing)
  turnaround_times_df <- turnaround_times |>
    rvest::html_elements("table") |>
    rvest::html_table()

  # Get individual dataframes
  film_turnaround_df <- turnaround_times_df[[1]]
  print_turnaround_df <- turnaround_times_df[[2]]

  # R
  if (stringr::str_to_lower(get_table) == "film") {
    return(film_turnaround_df)
  }
  if (stringr::str_to_lower(get_table) == "print") {
    return(print_turnaround_df)
  }

  rlang::abort("Unknown parameter provided to argument `get_table`")
  return(NA)
}
