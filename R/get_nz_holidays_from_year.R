get_nz_holidays_from_year <- function(year, api_key) {

  # Create API request
  holiday_req <- httr2::request("https://api.public-holidays.nz/v1/") |>
    httr2::req_url_path_append(
      path = "year"
    ) |>
    httr2::req_url_query(
      apikey = api_key,
      year = year
    )

  # Perform request
  holiday_resp <- holiday_req |> httr2::req_perform()

  # Process response into dataframe
  holidays_df <- holiday_resp |>
    httr2::resp_body_json(
      check_type = FALSE, # API response doesn't include a content type for some reason. Function fails because of this, so need to tell function not to check for the content type.
      simplifyVector = TRUE
    ) |>
    # Convert to tibble dataframe
    tibble::tibble() |> 
    # Convert strings to datetimes
    dplyr::mutate(ObservedDate = lubridate::dmy(ObservedDate)) |> 
    dplyr::mutate(ActualDate = lubridate::dmy(ActualDate))

  return(holidays_df)
}
