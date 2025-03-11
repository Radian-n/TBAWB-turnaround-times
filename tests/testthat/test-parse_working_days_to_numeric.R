test_that("Function parses strings", {
  c41_string <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = "2 Working Days",
    "Dev + Scan" = "5 Working Days",
    "Prints Add On" = "+ 1 Working Days"
  )

  result <- parse_working_days_to_numeric(c41_string)

  expected <- tibble::tibble(
    'Service' = "C-41",
    'Develop Only' = 2,
    'Dev + Scan' = 5,
    'Prints Add On' = 6
  )

  expect_equal(result, expected)
})


test_that("Function is case insensitive", {
  c41_string <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = "2 working Days",
    "Dev + Scan" = "5working days",
    "Prints Add On" = "+ 1 Working days"
  )

  result <- parse_working_days_to_numeric(c41_string)

  expected <- tibble::tibble(
    'Service' = "C-41",
    'Develop Only' = 2,
    'Dev + Scan' = 5,
    'Prints Add On' = 6
  )

  expect_equal(result, expected)
})


test_that("Function parameter checks work correctly", {
  # Must be of type dataframe
  expect_error(parse_working_days_to_numeric("not a dataframe"))

  # Dataframe must have only 3 columns
  test_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = "2 working Days",
    "Dev + Scan" = "5working days"
  )
  expect_error(parse_working_days_to_numeric(test_df))

  # Dataframe column names must be correct
  test_df <- tibble::tibble(
    "Service" = "C-41",
    "Develop Only" = "2 working Days",
    "Incorrec column name" = "5working days",
    "Prints Add On" = "+ 1 Working days"
  )
  expect_error(parse_working_days_to_numeric(test_df))

  # Function rejects E-6 dataframe row
  test_df <- tibble::tibble(
    "Service" = "E-6",
    "Develop Only" = "Processed Wednesday",
    "Dev + Scan" = "Scans Sent Friday",
    "Prints Add On" = "+ 1 Working days"
  )
  expect_error(parse_working_days_to_numeric(test_df))
})
