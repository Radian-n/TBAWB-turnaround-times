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
  # Scrape turnaround times page
  turnaround_times <- rvest::read_html(
    "https://theblackandwhitebox.co.nz/turn-around-times/"
  )

  tables <- rvest::html_elements(turnaround_times, "table")

  message("Found ", length(tables), " tables")
  message("Page title: ",
          rvest::html_element(turnaround_times, "title") |>
          rvest::html_text2())
  message("First 500 chars:")
  message(substr(as.character(tables), 1, 500))

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
