test_that("Function detects all days", {

  result <- extract_weekday_from_string("This should return 'Monday'")
  expect_equal(result, "Monday")
  result <- extract_weekday_from_string("This should return 'Tuesday'")
  expect_equal(result, "Tuesday")
  result <- extract_weekday_from_string("This should return 'Wednesday'")
  expect_equal(result, "Wednesday")
  result <- extract_weekday_from_string("This should return 'Thursday'")
  expect_equal(result, "Thursday")
  result <- extract_weekday_from_string("This should return 'Friday'")
  expect_equal(result, "Friday")
  result <- extract_weekday_from_string("This should return 'Saturday'")
  expect_equal(result, "Saturday")
  result <- extract_weekday_from_string("This should return 'Sunday'")
  expect_equal(result, "Sunday")

})


test_that("Function returns NA for non-weekday", {

  # Misspelled Wednesday
  result <- extract_weekday_from_string("This should return 'Wensday'")
  expect_equal(result, NA_character_)

  # No weekday
  result <- extract_weekday_from_string("Nothing here")
  expect_equal(result, NA_character_)
  
})


test_that("Function case insensitive", {

  result <- extract_weekday_from_string("This should return monday")
  expect_equal(result, "Monday")
  result <- extract_weekday_from_string("This should return tUesDaY")
  expect_equal(result, "Tuesday")
  result <- extract_weekday_from_string("This should return WEDNESDAY")
  expect_equal(result, "Wednesday")


})
