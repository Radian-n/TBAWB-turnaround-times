extract_weekday_from_string <- function(string) {

  # Conver to lowercase to ensure matching not case-sensative
  string_lower <- stringr::str_to_lower(string)

  # Extract weekday name from string
  weekday <- stringr::str_extract(string_lower, "\\b(monday|tuesday|wednesday|thursday|friday|saturday|sunday)\\b")

  # Ensure returned string is title case (e.g starts with a capital letter)
  weekday <- stringr::str_to_title(weekday)

  return(weekday)

}