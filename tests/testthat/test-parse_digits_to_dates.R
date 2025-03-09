test_that("No holidays no weekend", {
  input_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = 1,
    "Dev + Scan" = 2,
    "Prints Add On" = 3
  )

  test_date <- lubridate::ymd("2025-03-10")

  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday")
  )

  result_df <- parse_digits_to_dates(
    input_df,
    biz_calendar_name = "test_cal",
    current_date = test_date
  )

  expected_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = lubridate::ymd("2025-03-11"),
    "Dev + Scan" = lubridate::ymd("2025-03-12"),
    "Prints Add On" = lubridate::ymd("2025-03-13")
  )

  expect_equal(result_df, expected_df)
})


test_that("Over weekend", {

  input_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = 1,
    "Dev + Scan" = 2,
    "Prints Add On" = 3
  )

  test_date <- lubridate::ymd("2025-03-14")

  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday")
  )

  result_df <- parse_digits_to_dates(
    input_df,
    biz_calendar_name = "test_cal",
    current_date = test_date
  )

  expected_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = lubridate::ymd("2025-03-17"),
    "Dev + Scan" = lubridate::ymd("2025-03-18"),
    "Prints Add On" = lubridate::ymd("2025-03-19")
  )

  expect_equal(result_df, expected_df)
})


test_that("Over single holiday", {

  input_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = 1,
    "Dev + Scan" = 2,
    "Prints Add On" = 3
  )

  test_date <- lubridate::ymd("2025-03-10")

  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = lubridate::ymd("2025-03-11"),
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  result_df <- parse_digits_to_dates(
    input_df,
    biz_calendar_name = "test_cal",
    current_date = test_date
  )

  expected_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = lubridate::ymd("2025-03-12"),
    "Dev + Scan" = lubridate::ymd("2025-03-13"),
    "Prints Add On" = lubridate::ymd("2025-03-14")
  )

  expect_equal(result_df, expected_df)
})


test_that("Over long weekend", {

  input_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = 3,
    "Dev + Scan" = 4,
    "Prints Add On" = 5
  )

  test_date <- lubridate::ymd("2025-03-12")

  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = lubridate::ymd("2025-03-17"),
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  result_df <- parse_digits_to_dates(
    input_df,
    biz_calendar_name = "test_cal",
    current_date = test_date
  )

  expected_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = lubridate::ymd("2025-03-18"),
    "Dev + Scan" = lubridate::ymd("2025-03-19"),
    "Prints Add On" = lubridate::ymd("2025-03-20")
  )

  expect_equal(result_df, expected_df)
})


test_that("New Years period", {

  input_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = 3,
    "Dev + Scan" = 4,
    "Prints Add On" = 5
  )

  test_date <- lubridate::ymd("2025-12-28")
  new_year_holidays <- c("2025-12-31", "2026-01-01", "2026-01-02")
  new_year_holidays <- lubridate::ymd(new_year_holidays)

  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = new_year_holidays,
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  result_df <- parse_digits_to_dates(
    input_df,
    biz_calendar_name = "test_cal",
    current_date = test_date
  )

  expected_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = lubridate::ymd("2026-01-05"),
    "Dev + Scan" = lubridate::ymd("2026-01-06"),
    "Prints Add On" = lubridate::ymd("2026-01-07")
  )

  expect_equal(result_df, expected_df)
})


test_that("Function parameter checks work correctly", {
  test_date <- lubridate::ymd("2025-03-10")
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday")
  )

  input_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = 1,
    "Dev + Scan" = 2,
    "Prints Add On" = 3
  )

  # Check dataframe
  expect_error(
    parse_digits_to_dates(
      "not a dataframe",
      biz_calendar_name = "test_cal",
      current_date = test_date
    )
  )
  # Missing column
  expect_error(
    parse_digits_to_dates(
      input_df[, -3],
      biz_calendar_name = "test_cal",
      current_date = test_date
    )
  )
  # More than 1 row
  expect_error(
    parse_digits_to_dates(
      input_df |> dplyr::add_row(input_df[1, ]),
      biz_calendar_name = "test_cal",
      current_date = test_date
    )
  )
  # Incorrect column name
  expect_error(
    parse_digits_to_dates(
      input_df |> dplyr::rename(wrong_name = `Prints Add On`),
      biz_calendar_name = "test_cal",
      current_date = test_date
    )
  )
  # E-6 row not allowed
  expect_error(
    parse_digits_to_dates(
      input_df |> dplyr::mutate(`Service` = "E-6"),
      biz_calendar_name = "test_cal",
      current_date = test_date
    )
  )
  # biz_calendar_name valid
  expect_error(
    parse_digits_to_dates(
      input_df,
      biz_calendar_name = "not an existing calendar",
      current_date = test_date
    )
  )
  # current_date is a Date
  expect_error(
    parse_digits_to_dates(
      input_df,
      biz_calendar_name = "test_cal",
      current_date = "Not a date"
    )
  )
  # current_date is a single date
  expect_error(
    parse_digits_to_dates(
      input_df,
      biz_calendar_name = "test_cal",
      current_date = c(test_date, test_date)
    )
  )
})
