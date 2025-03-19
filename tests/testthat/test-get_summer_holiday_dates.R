test_that("2025 january checks", {
  # Summer period ends on a Saturday
  result_v <- get_summer_holiday_dates(2025, december_start_day = 27, january_end_day = 4)
  expected_v <- c(
    "2025-01-01",
    "2025-01-02",
    "2025-01-03",
    "2025-01-04",
    "2025-01-05",
    "2025-12-27",
    "2025-12-28",
    "2025-12-29",
    "2025-12-30",
    "2025-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Sunday
  result_v <- get_summer_holiday_dates(2025, december_start_day = 27, january_end_day = 5)
  expected_v <- c(
    "2025-01-01",
    "2025-01-02",
    "2025-01-03",
    "2025-01-04",
    "2025-01-05",
    "2025-12-27",
    "2025-12-28",
    "2025-12-29",
    "2025-12-30",
    "2025-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Monday
  result_v <- get_summer_holiday_dates(2025, december_start_day = 27, january_end_day = 6)
  expected_v <- c(
    "2025-01-01",
    "2025-01-02",
    "2025-01-03",
    "2025-01-04",
    "2025-01-05",
    "2025-12-27",
    "2025-12-28",
    "2025-12-29",
    "2025-12-30",
    "2025-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Tuesday
  result_v <- get_summer_holiday_dates(2025, december_start_day = 27, january_end_day = 7)
  expected_v <- c(
    "2025-01-01",
    "2025-01-02",
    "2025-01-03",
    "2025-01-04",
    "2025-01-05",
    "2025-01-06",
    "2025-12-27",
    "2025-12-28",
    "2025-12-29",
    "2025-12-30",
    "2025-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)
})


test_that("2026 January checks", {
  
  # Summer period ends on a Sunday
  result_v <- get_summer_holiday_dates(2026, december_start_day = 27, january_end_day = 3)
  expected_v <- c(
    "2026-01-01",
    "2026-01-02",
    "2026-01-03",
    "2026-01-04",
    "2026-12-27",
    "2026-12-28",
    "2026-12-29",
    "2026-12-30",
    "2026-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Sunday
  result_v <- get_summer_holiday_dates(2026, december_start_day = 27, january_end_day = 4)
  expected_v <- c(
    "2026-01-01",
    "2026-01-02",
    "2026-01-03",
    "2026-01-04",
    "2026-12-27",
    "2026-12-28",
    "2026-12-29",
    "2026-12-30",
    "2026-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Monday
  result_v <- get_summer_holiday_dates(2026, december_start_day = 27, january_end_day = 5)
  expected_v <- c(
    "2026-01-01",
    "2026-01-02",
    "2026-01-03",
    "2026-01-04",
    "2026-12-27",
    "2026-12-28",
    "2026-12-29",
    "2026-12-30",
    "2026-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Tuesday
  result_v <- get_summer_holiday_dates(2026, december_start_day = 27, january_end_day = 6)
  expected_v <- c(
    "2026-01-01",
    "2026-01-02",
    "2026-01-03",
    "2026-01-04",
    "2026-01-05",
    "2026-12-27",
    "2026-12-28",
    "2026-12-29",
    "2026-12-30",
    "2026-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Tuesday
  result_v <- get_summer_holiday_dates(2026, december_start_day = 27, january_end_day = 7)
  expected_v <- c(
    "2026-01-01",
    "2026-01-02",
    "2026-01-03",
    "2026-01-04",
    "2026-01-05",
    "2026-01-06",
    "2026-12-27",
    "2026-12-28",
    "2026-12-29",
    "2026-12-30",
    "2026-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)
})


test_that("2027 January checks", {
  
  # Summer period ends on a Sunday
  result_v <- get_summer_holiday_dates(2027, december_start_day = 27, january_end_day = 3)
  expected_v <- c(
    "2027-01-01",
    "2027-01-02",
    "2027-01-03",
    "2027-12-27",
    "2027-12-28",
    "2027-12-29",
    "2027-12-30",
    "2027-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Sunday
  result_v <- get_summer_holiday_dates(2027, december_start_day = 27, january_end_day = 4)
  expected_v <- c(
    "2027-01-01",
    "2027-01-02",
    "2027-01-03",
    "2027-12-27",
    "2027-12-28",
    "2027-12-29",
    "2027-12-30",
    "2027-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Monday
  result_v <- get_summer_holiday_dates(2027, december_start_day = 27, january_end_day = 5)
  expected_v <- c(
    "2027-01-01",
    "2027-01-02",
    "2027-01-03",
    "2027-01-04",
    "2027-12-27",
    "2027-12-28",
    "2027-12-29",
    "2027-12-30",
    "2027-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Tuesday
  result_v <- get_summer_holiday_dates(2027, december_start_day = 27, january_end_day = 6)
  expected_v <- c(
    "2027-01-01",
    "2027-01-02",
    "2027-01-03",
    "2027-01-04",
    "2027-01-05",
    "2027-12-27",
    "2027-12-28",
    "2027-12-29",
    "2027-12-30",
    "2027-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)

  # Summer period ends on a Tuesday
  result_v <- get_summer_holiday_dates(2027, december_start_day = 27, january_end_day = 7)
  expected_v <- c(
    "2027-01-01",
    "2027-01-02",
    "2027-01-03",
    "2027-01-04",
    "2027-01-05",
    "2027-01-06",
    "2027-12-27",
    "2027-12-28",
    "2027-12-29",
    "2027-12-30",
    "2027-12-31"
  )
  expected_v <- lubridate::ymd(expected_v)
  expect_equal(result_v, expected_v)
})
