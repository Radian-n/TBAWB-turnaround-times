test_that("Film arrives Saturday Sunday Monday Tuesday", {
  # Setup mock date and test calendar. These are required by the function.
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday")
  )
  # Example of an input dataframe
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Satuday
  saturday <- lubridate::ymd("2025-03-08")
  result <- parse_weekdays_to_date(
    e6_string_df,
    biz_calendar_name = "test_cal",
    date_today = saturday
  )
  expected <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-13"),
    "Dev + Scan" = lubridate::ymd("2025-03-14"),
    "Prints Add On" = lubridate::ymd("2025-03-17")
  )
  expect_equal(result, expected)

  # Sunday
  sunday <- lubridate::ymd("2025-03-09")
  result <- parse_weekdays_to_date(
    e6_string_df,
    biz_calendar_name = "test_cal",
    date_today = sunday
  )
  expected <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-13"),
    "Dev + Scan" = lubridate::ymd("2025-03-14"),
    "Prints Add On" = lubridate::ymd("2025-03-17")
  )
  expect_equal(result, expected)

  # Monday
  monday <- lubridate::ymd("2025-03-10")
  result <- parse_weekdays_to_date(
    e6_string_df,
    biz_calendar_name = "test_cal",
    date_today = monday
  )
  expected <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-13"),
    "Dev + Scan" = lubridate::ymd("2025-03-14"),
    "Prints Add On" = lubridate::ymd("2025-03-17")
  )
  expect_equal(result, expected)

  # Tuesday
  tuesday <- lubridate::ymd("2025-03-11")
  result <- parse_weekdays_to_date(
    e6_string_df,
    biz_calendar_name = "test_cal",
    date_today = tuesday
  )
  expected <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-13"),
    "Dev + Scan" = lubridate::ymd("2025-03-14"),
    "Prints Add On" = lubridate::ymd("2025-03-17")
  )
  expect_equal(result, expected)
})


test_that("Film arrives Wednesday", {
  # In this case, the due dates for this order should be NEXT wednesday & friday.
  wednesday <- lubridate::ymd("2025-03-12")
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday")
  )
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  result <- parse_weekdays_to_date(
    e6_string_df,
    biz_calendar_name = "test_cal",
    date_today = wednesday
  )
  expected <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-20"),
    "Dev + Scan" = lubridate::ymd("2025-03-21"),
    "Prints Add On" = lubridate::ymd("2025-03-24")
  )
  expect_equal(result, expected)
})

test_that("Film arrives Thursday", {
  # In this case, the due dates for this order should be NEXT wednesday & friday.
  thursday <- lubridate::ymd("2025-03-13")
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday")
  )
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  result <- parse_weekdays_to_date(
    e6_string_df,
    biz_calendar_name = "test_cal",
    date_today = thursday
  )
  expected <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-20"),
    "Dev + Scan" = lubridate::ymd("2025-03-21"),
    "Prints Add On" = lubridate::ymd("2025-03-24")
  )
  expect_equal(result, expected)
})

test_that("Film arrives Friday", {
  # In this case, the due dates for this order should be NEXT wednesday & friday.
  friday <- lubridate::ymd("2025-03-13")
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday")
  )
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  result <- parse_weekdays_to_date(
    e6_string_df,
    biz_calendar_name = "test_cal",
    date_today = friday
  )
  expected <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-20"),
    "Dev + Scan" = lubridate::ymd("2025-03-21"),
    "Prints Add On" = lubridate::ymd("2025-03-24")
  )
  expect_equal(result, expected)
})



test_that("Function parameter checks work correctly", {
  test_date <- lubridate::ymd("2025-03-10")
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday")
  )

  input_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Check dataframe
  expect_error(
    parse_weekdays_to_date(
      "not a dataframe",
      biz_calendar_name = "test_cal",
      date_today = test_date
    ),
    regexp = "`process_row_df` must be a dataframe"
  )
  # Missing column
  expect_error(
    parse_weekdays_to_date(
      input_df[, -3],
      biz_calendar_name = "test_cal",
      date_today = test_date
    ),
    regexp = "`process_row_df` must have 1 row and 4 columns"
  )
  # More than 1 row
  expect_error(
    parse_weekdays_to_date(
      input_df |> dplyr::add_row(input_df[1, ]),
      biz_calendar_name = "test_cal",
      date_today = test_date
    ),
    regexp = "`process_row_df` must have 1 row and 4 columns"
  )
  # Incorrect column name
  expect_error(
    parse_weekdays_to_date(
      input_df |> dplyr::rename(wrong_name = `Prints Add On`),
      biz_calendar_name = "test_cal",
      date_today = test_date
    ),
    regexp = "`process_row_df` must have column names:.*"
  )
  # E-6 row not allowed
  expect_error(
    parse_weekdays_to_date(
      input_df |> dplyr::mutate(`Service` = "C-41"),
      biz_calendar_name = "test_cal",
      date_today = test_date
    ),
    regexp = "This function should only be used to handle E-6 turnaround times.*"
  )
  # biz_calendar_name type check
  expect_error(
    parse_weekdays_to_date(
      input_df,
      biz_calendar_name = test_cal, # Calendar object, not string.
      date_today = test_date
    ),
    regexp = "`biz_calendar_name` must be a string.*"
  )
  # biz_calendar_name valid
  expect_error(
    parse_weekdays_to_date(
      input_df,
      biz_calendar_name = "not an existing calendar",
      date_today = test_date
    ),
    regexp = "`biz_calendar_name` must be a valid bizdays calendar.*"
  )
  # date_today is a Date
  expect_error(
    parse_weekdays_to_date(
      input_df,
      biz_calendar_name = "test_cal",
      date_today = "Not a date"
    ),
    regexp = "`date_today` must be of type Date.*"
  )
  # date_today is a single date
  expect_error(
    parse_weekdays_to_date(
      input_df,
      biz_calendar_name = "test_cal",
      date_today = c(test_date, test_date)
    ),
    regexp = "`date_today` must be a single date.*"
  )
})
