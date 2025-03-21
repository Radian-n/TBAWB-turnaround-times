#' Calls the NZ Holidays API and returns a dataframe of holidays
#'
#' @param api_key A single string. The API key for NZ Holidays.
#'
#' @returns A dataframe.
#' @export
get_all_nz_holidays <- function(api_key) {
  # Create API request
  holiday_req <- httr2::request("https://api.public-holidays.nz/v1/") |>
    httr2::req_url_path_append(
      path = "all"
    ) |>
    httr2::req_url_query(
      apikey = api_key
    )

  # Perform request
  holiday_resp <- holiday_req |> httr2::req_perform()

  # Process response into dataframe
  holidays_df <- holiday_resp |>
    httr2::resp_body_json(
      check_type = FALSE,  # API response doesn't include content-type=application/json.
      simplifyVector = TRUE
    ) |>
    # Convert to tibble dataframe
    tibble::tibble() |>
    # Convert strings to datetimes
    dplyr::mutate(ObservedDate = lubridate::dmy(ObservedDate)) |>
    dplyr::mutate(ActualDate = lubridate::dmy(ActualDate))

  return(holidays_df)
}
