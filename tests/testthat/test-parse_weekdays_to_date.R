# ================== NO PUBLIC HOLIDAYS ==================

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
    date_today = saturday,
    biz_calendar_name = "test_cal"
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
    date_today = sunday,
    biz_calendar_name = "test_cal"
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
    date_today = monday,
    biz_calendar_name = "test_cal"
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
    date_today = tuesday,
    biz_calendar_name = "test_cal"
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
  # The due dates for this order should be NEXT wednesday & friday.
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
    date_today = wednesday,
    biz_calendar_name = "test_cal"
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
  # The due dates for this order should be NEXT wednesday & friday.
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
    date_today = thursday,
    biz_calendar_name = "test_cal"
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
  # The due dates for this order should be NEXT wednesday & friday.
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
    date_today = friday,
    biz_calendar_name = "test_cal"
  )
  expected <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-20"),
    "Dev + Scan" = lubridate::ymd("2025-03-21"),
    "Prints Add On" = lubridate::ymd("2025-03-24")
  )
  expect_equal(result, expected)
})


# ================== PUBLIC HOLIDAYS ==================

test_that("Wednesday public holiday", {

  # Setup E-6 turnaround time example
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Setup calendar with public holiday on Wednesday
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = c(lubridate::ymd("2025-03-12")), # Wednesday holiday
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )


  # Film arrived last Wednesday
  last_wednesday <- lubridate::ymd("2025-03-5")
  result_last_wednesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_wednesday,
    biz_calendar_name = "test_cal"
  )
  # Film arrived last Thursday
  last_thursday <- lubridate::ymd("2025-03-6")
  result_last_thursday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_thursday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived last Friday
  last_friday <- lubridate::ymd("2025-03-7")
  result_last_friday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_friday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Saturday
  saturday <- lubridate::ymd("2025-03-8")
  result_saturday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = saturday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Sunday
  sunday <- lubridate::ymd("2025-03-9")
  result_sunday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = sunday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Monday
  monday <- lubridate::ymd("2025-03-10")
  result_monday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = monday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Tuesday
  tuesday <- lubridate::ymd("2025-03-11")
  result_tuesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = tuesday,
    biz_calendar_name = "test_cal"
  )

  # Regardless of arrival day, due dates should be identical
  expected_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-14"),
    "Dev + Scan" = lubridate::ymd("2025-03-17"),
    "Prints Add On" = lubridate::ymd("2025-03-18")
  )

  # Tests
  expect_equal(result_last_wednesday, expected_df)
  expect_equal(result_last_thursday, expected_df)
  expect_equal(result_last_friday, expected_df)
  expect_equal(result_saturday, expected_df)
  expect_equal(result_sunday, expected_df)
  expect_equal(result_monday, expected_df)
  expect_equal(result_tuesday, expected_df)
})


test_that("Thursday public holiday", {
  # Setup E-6 turnaround time example
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Setup calendar with public holiday on Wednesday
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = c(lubridate::ymd("2025-03-13")), # Thursday holiday
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  # Film arrived last Wednesday
  last_wednesday <- lubridate::ymd("2025-03-5")
  result_last_wednesday <- parse_weekdays_to_date(
    e6_string_df,
    biz_calendar_name = "test_cal",
    date_today = last_wednesday
  )
  # Film arrived last Thursday
  last_thursday <- lubridate::ymd("2025-03-6")
  result_last_thursday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_thursday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived last Friday
  last_friday <- lubridate::ymd("2025-03-7")
  result_last_friday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_friday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Saturday
  saturday <- lubridate::ymd("2025-03-8")
  result_saturday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = saturday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Sunday
  sunday <- lubridate::ymd("2025-03-9")
  result_sunday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = sunday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Monday
  monday <- lubridate::ymd("2025-03-10")
  result_monday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = monday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Tuesday
  tuesday <- lubridate::ymd("2025-03-11")
  result_tuesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = tuesday,
    biz_calendar_name = "test_cal"
  )

  # Regardless of arrival day, due dates should be identical
  expected_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-14"),
    "Dev + Scan" = lubridate::ymd("2025-03-17"),
    "Prints Add On" = lubridate::ymd("2025-03-18")
  )

  # Tests
  expect_equal(result_last_wednesday, expected_df)
  expect_equal(result_last_thursday, expected_df)
  expect_equal(result_last_friday, expected_df)
  expect_equal(result_saturday, expected_df)
  expect_equal(result_sunday, expected_df)
  expect_equal(result_monday, expected_df)
  expect_equal(result_tuesday, expected_df)
})

test_that("Friday public holiday", {
  # Setup E-6 turnaround time example
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Setup calendar with public holiday on Wednesday
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = c(lubridate::ymd("2025-03-14")), # Friday holiday
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  # Film arrived last Wednesday
  last_wednesday <- lubridate::ymd("2025-03-5")
  result_last_wednesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_wednesday,
    biz_calendar_name = "test_cal"
  )
  # Film arrived last Thursday
  last_thursday <- lubridate::ymd("2025-03-6")
  result_last_thursday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_thursday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived last Friday
  last_friday <- lubridate::ymd("2025-03-7")
  result_last_friday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_friday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Saturday
  saturday <- lubridate::ymd("2025-03-8")
  result_saturday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = saturday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Sunday
  sunday <- lubridate::ymd("2025-03-9")
  result_sunday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = sunday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Monday
  monday <- lubridate::ymd("2025-03-10")
  result_monday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = monday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Tuesday
  tuesday <- lubridate::ymd("2025-03-11")
  result_tuesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = tuesday,
    biz_calendar_name = "test_cal"
  )

  # Regardless of arrival day, due dates should be identical
  expected_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-13"),
    "Dev + Scan" = lubridate::ymd("2025-03-17"),
    "Prints Add On" = lubridate::ymd("2025-03-18")
  )

  # Tests
  expect_equal(result_last_wednesday, expected_df)
  expect_equal(result_last_thursday, expected_df)
  expect_equal(result_last_friday, expected_df)
  expect_equal(result_saturday, expected_df)
  expect_equal(result_sunday, expected_df)
  expect_equal(result_monday, expected_df)
  expect_equal(result_tuesday, expected_df)
})


test_that("Wednesday & Thursday are public holiday", {
  # Setup E-6 turnaround time example
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Setup calendar with public holiday on Wednesday
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = lubridate::ymd(c("2025-03-12"), c("2025-03-13")), # Friday holiday
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  # Film arrived last Wednesday
  last_wednesday <- lubridate::ymd("2025-03-5")
  result_last_wednesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_wednesday,
    biz_calendar_name = "test_cal"
  )
  # Film arrived last Thursday
  last_thursday <- lubridate::ymd("2025-03-6")
  result_last_thursday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_thursday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived last Friday
  last_friday <- lubridate::ymd("2025-03-7")
  result_last_friday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_friday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Saturday
  saturday <- lubridate::ymd("2025-03-8")
  result_saturday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = saturday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Sunday
  sunday <- lubridate::ymd("2025-03-9")
  result_sunday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = sunday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Monday
  monday <- lubridate::ymd("2025-03-10")
  result_monday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = monday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Tuesday
  tuesday <- lubridate::ymd("2025-03-11")
  result_tuesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = tuesday,
    biz_calendar_name = "test_cal"
  )

  # Regardless of arrival day, due dates should be identical
  expected_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-17"),
    "Dev + Scan" = lubridate::ymd("2025-03-18"),
    "Prints Add On" = lubridate::ymd("2025-03-19")
  )

  # Tests
  expect_equal(result_last_wednesday, expected_df)
  expect_equal(result_last_thursday, expected_df)
  expect_equal(result_last_friday, expected_df)
  expect_equal(result_saturday, expected_df)
  expect_equal(result_sunday, expected_df)
  expect_equal(result_monday, expected_df)
  expect_equal(result_tuesday, expected_df)
})


test_that("Thursday & Friday are public holiday", {
  # Setup E-6 turnaround time example
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Setup calendar with public holiday on Wednesday
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = lubridate::ymd(c("2025-03-13"), c("2025-03-14")), # Friday holiday
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  # Film arrived last Wednesday
  last_wednesday <- lubridate::ymd("2025-03-5")
  result_last_wednesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_wednesday,
    biz_calendar_name = "test_cal"
  )
  # Film arrived last Thursday
  last_thursday <- lubridate::ymd("2025-03-6")
  result_last_thursday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_thursday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived last Friday
  last_friday <- lubridate::ymd("2025-03-7")
  result_last_friday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_friday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Saturday
  saturday <- lubridate::ymd("2025-03-8")
  result_saturday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = saturday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Sunday
  sunday <- lubridate::ymd("2025-03-9")
  result_sunday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = sunday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Monday
  monday <- lubridate::ymd("2025-03-10")
  result_monday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = monday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Tuesday
  tuesday <- lubridate::ymd("2025-03-11")
  result_tuesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = tuesday,
    biz_calendar_name = "test_cal"
  )

  # Regardless of arrival day, due dates should be identical
  expected_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-17"),
    "Dev + Scan" = lubridate::ymd("2025-03-18"),
    "Prints Add On" = lubridate::ymd("2025-03-19")
  )

  # Tests
  expect_equal(result_last_wednesday, expected_df)
  expect_equal(result_last_thursday, expected_df)
  expect_equal(result_last_friday, expected_df)
  expect_equal(result_saturday, expected_df)
  expect_equal(result_sunday, expected_df)
  expect_equal(result_monday, expected_df)
  expect_equal(result_tuesday, expected_df)
})


test_that("Wednesday & Thursday & Friday are public holiday", {
  # Setup E-6 turnaround time example
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Setup calendar with public holiday on Wednesday
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = lubridate::ymd(c("2025-03-12"), c("2025-03-13"), c("2025-03-14")), # Friday holiday
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  # Film arrived last Wednesday
  last_wednesday <- lubridate::ymd("2025-03-5")
  result_last_wednesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_wednesday,
    biz_calendar_name = "test_cal"
  )
  # Film arrived last Thursday
  last_thursday <- lubridate::ymd("2025-03-6")
  result_last_thursday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_thursday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived last Friday
  last_friday <- lubridate::ymd("2025-03-7")
  result_last_friday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_friday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Saturday
  saturday <- lubridate::ymd("2025-03-8")
  result_saturday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = saturday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Sunday
  sunday <- lubridate::ymd("2025-03-9")
  result_sunday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = sunday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Monday
  monday <- lubridate::ymd("2025-03-10")
  result_monday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = monday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Tuesday
  tuesday <- lubridate::ymd("2025-03-11")
  result_tuesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = tuesday,
    biz_calendar_name = "test_cal"
  )

  # Regardless of arrival day, due dates should be identical
  expected_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-18"),
    "Dev + Scan" = lubridate::ymd("2025-03-19"),
    "Prints Add On" = lubridate::ymd("2025-03-20")
  )

  # Tests
  expect_equal(result_last_wednesday, expected_df)
  expect_equal(result_last_thursday, expected_df)
  expect_equal(result_last_friday, expected_df)
  expect_equal(result_saturday, expected_df)
  expect_equal(result_sunday, expected_df)
  expect_equal(result_monday, expected_df)
  expect_equal(result_tuesday, expected_df)
})


test_that("Wednesday & Friday are public holiday", {
  # Setup E-6 turnaround time example
  e6_string_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working Days"
  )

  # Setup calendar with public holiday on Wednesday
  test_cal <- bizdays::create.calendar(
    "test_cal",
    weekdays = c("saturday", "sunday"),
    holidays = lubridate::ymd(c("2025-03-12"), c("2025-03-14")), # Friday holiday
    start.date = lubridate::ymd("1970-01-01"),
    end.date = lubridate::ymd("2071-01-01"),
  )

  # Film arrived last Wednesday
  last_wednesday <- lubridate::ymd("2025-03-5")
  result_last_wednesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_wednesday,
    biz_calendar_name = "test_cal"
  )
  # Film arrived last Thursday
  last_thursday <- lubridate::ymd("2025-03-6")
  result_last_thursday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_thursday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived last Friday
  last_friday <- lubridate::ymd("2025-03-7")
  result_last_friday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = last_friday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Saturday
  saturday <- lubridate::ymd("2025-03-8")
  result_saturday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = saturday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Sunday
  sunday <- lubridate::ymd("2025-03-9")
  result_sunday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = sunday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Monday
  monday <- lubridate::ymd("2025-03-10")
  result_monday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = monday,
    biz_calendar_name = "test_cal"
  )

  # Film arrived Tuesday
  tuesday <- lubridate::ymd("2025-03-11")
  result_tuesday <- parse_weekdays_to_date(
    e6_string_df,
    date_today = tuesday,
    biz_calendar_name = "test_cal"
  )

  # Regardless of arrival day, due dates should be identical
  expected_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = lubridate::ymd("2025-03-17"),
    "Dev + Scan" = lubridate::ymd("2025-03-18"),
    "Prints Add On" = lubridate::ymd("2025-03-19")
  )

  # Tests
  expect_equal(result_last_wednesday, expected_df)
  expect_equal(result_last_thursday, expected_df)
  expect_equal(result_last_friday, expected_df)
  expect_equal(result_saturday, expected_df)
  expect_equal(result_sunday, expected_df)
  expect_equal(result_monday, expected_df)
  expect_equal(result_tuesday, expected_df)
})


# ================== ARGUMENT CHECKS ==================

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
      date_today = test_date,
      biz_calendar_name = "test_cal"
    ),
    regexp = "`process_row_df` must be a dataframe"
  )
  # Missing column
  expect_error(
    parse_weekdays_to_date(
      input_df[, -3],
      date_today = test_date,
      biz_calendar_name = "test_cal"
    ),
    regexp = "`process_row_df` must have 1 row and 4 columns"
  )
  # More than 1 row
  expect_error(
    parse_weekdays_to_date(
      input_df |> dplyr::add_row(input_df[1, ]),
      date_today = test_date,
      biz_calendar_name = "test_cal"
    ),
    regexp = "`process_row_df` must have 1 row and 4 columns"
  )
  # Incorrect column name
  expect_error(
    parse_weekdays_to_date(
      input_df |> dplyr::rename(wrong_name = `Prints Add On`),
      date_today = test_date,
      biz_calendar_name = "test_cal"
    ),
    regexp = "`process_row_df` must have column names:.*"
  )
  # E-6 row not allowed
  expect_error(
    parse_weekdays_to_date(
      input_df |> dplyr::mutate(`Service` = "C-41"),
      date_today = test_date,
      biz_calendar_name = "test_cal"
    ),
    regexp = "This function should only be used to handle E-6 turnaround time.*"
  )
  # biz_calendar_name type check
  expect_error(
    parse_weekdays_to_date(
      input_df,
      date_today = test_date,
      biz_calendar_name = test_cal  # Calendar object, not string.
    ),
    regexp = "`biz_calendar_name` must be a string.*"
  )
  # biz_calendar_name valid
  expect_error(
    parse_weekdays_to_date(
      input_df,
      date_today = test_date,
      biz_calendar_name = "not an existing calendar"
    ),
    regexp = "`biz_calendar_name` must be a valid bizdays calendar.*"
  )
  # date_today is a Date
  expect_error(
    parse_weekdays_to_date(
      input_df,
      date_today = "Not a date",
      biz_calendar_name = "test_cal"
    ),
    regexp = "`date_today` must be of type Date.*"
  )
  # date_today is a single date
  expect_error(
    parse_weekdays_to_date(
      input_df,
      date_today = c(test_date, test_date),
      biz_calendar_name = "test_cal"
    ),
    regexp = "`date_today` must be a single date.*"
  )
})
