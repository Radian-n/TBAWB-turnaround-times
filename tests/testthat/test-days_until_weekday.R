test_that("Dates calculated correctly", {

  # Monday -> Wednesday = 2
  monday <- lubridate::ymd("2025-03-10")
  result <- days_until_weekday("Wednesday", date_from = monday)
  expect_equal(result, 2)

  # Wednesday -> Wednesday = 7
  wednesday <- lubridate::ymd("2025-03-12")
  result <- days_until_weekday("Wednesday", date_from = wednesday)
  expect_equal(result, 7)

  # Month rollover
  friday <- lubridate::ymd("2025-01-31")
  result <- days_until_weekday("Saturday", date_from = friday)
  expect_equal(result, 1)

  # Year rollover
  wednesday <- lubridate::ymd("2025-12-31")
  result <- days_until_weekday("Friday", date_from = wednesday)
  expect_equal(result, 2)

  # Leap year
  wednesday <- lubridate::ymd("2024-02-28")
  result <- days_until_weekday("Friday", date_from = wednesday)
  expect_equal(result, 2)

})



test_that("Weekday names spelled correctly", {

  today <- lubridate::ymd("2025-03-10")
  weekday_strings <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")

  # Run all the weekday names through the function. If any weekday names spelled incorrectly inside assert_that() check, this should return an error.
  result <- map(
    weekday_strings, 
    ~days_until_weekday(weekday = .x, date_from = today)
  )

  expected <- list(7, 1, 2, 3, 4, 5, 6)
  expect_equal(result, expected)

})



test_that("Weekday names incorrectly capitalised", {
  monday <- lubridate::ymd("2025-03-10")
  weekday_string <- "monday"  # lowercase
  days_until_weekday(weekday_string, date_from = monday)
  expect_equal(result, 7)
})



test_that("Function parameter checks work correctly", {

  today <- lubridate::ymd("2025-03-10")
  # Check `weekday` type is string
  expect_error(days_until_weekday(123, date_from = today))
  # Check `weekday` type is singular
  expect_error(days_until_weekday(c("Monday", "Tuesday"), date_from = today))
  # Check `weekday` is a valid weekday
  expect_error(days_until_weekday("not a weekday", date_from = today))

  # Check `date_from` is of correct type
  expect_error(days_until_weekday("Monday", date_from = "2025-01-01"))
  # Check `date_from` type is singular
  expect_error(days_until_weekday("not a weekday", date_from = c(today, today)))
  
})
