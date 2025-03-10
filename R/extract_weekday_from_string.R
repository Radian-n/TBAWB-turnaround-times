#' Extracts the weekday name from a string.
#'
#' @param string A single string, possibily containing a weekday.
#'
#' @returns A single string (if a weekday exists in the string), else NA.
#' @export
#'
#' @examples
#' extract_weekday_from_string("This should return 'Monday'")
#' # [1] "Monday"
#' 
#' extract_weekday_from_string("This doesn't have a weekday")
#' # [1] NA
#' 
#' extract_weekday_from_string("Case insensitive: thuRsdaY")
#' # [1] "Thursday"
extract_weekday_from_string <- function(string) {

  # Conver to lowercase to ensure matching not case-sensative
  string_lower <- stringr::str_to_lower(string)

  # Extract weekday name from string
  weekday <- stringr::str_extract(string_lower, "\\b(monday|tuesday|wednesday|thursday|friday|saturday|sunday)\\b")

  # Ensure returned string is title case (e.g starts with a capital letter)
  weekday <- stringr::str_to_title(weekday)

  return(weekday)

}
