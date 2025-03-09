get_turnaround_times_df <- function(get_table = "film") {

  # Scrape turnaround times page
  turnaround_times <- rvest::read_html(
    "https://theblackandwhitebox.co.nz/turn-around-times/"
  )

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

} 
