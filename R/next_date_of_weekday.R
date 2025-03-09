# Function to find the next date for a given weekday
next_date_of_weekday <- function(
  weekday_name,
  start_date  = lubridate::today(tzone = "Pacific/Auckland")
) {
  
  # Convert the weekday name to lowercase for consistent comparison
  weekday_name <- tolower(weekday_name)

  # Validate the input weekday name
  valid_weekdays <- c("monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday")

  # Check that the weekday parameter is correct
  assertthat::assert_that(
    weekday_name %in% valid_weekdays,
    msg = glue::glue(
      "`weekday` must be a named weekday, not '{weekday}'. Ensure correct spelling"
    )
  )

  # Get the numeric value of the specified weekday (1 = Monday, ..., 7 = Sunday)
  target_weekday <- match(weekday_name, valid_weekdays)

  # Get the numeric value of the starting date's weekday
  start_weekday <- as.integer(format(start_date, "%u"))

  # Calculate the difference in days to reach the target weekday
  days_to_next <- (target_weekday - start_weekday + 7) %% 7
  if (days_to_next == 0) days_to_next <- 7 # Move to the next week's weekday if it's the same day

  # Calculate the date of the next target weekday
  next_date <- start_date + days_to_next

  return(next_date)
}
