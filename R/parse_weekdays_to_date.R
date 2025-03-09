parse_weekdays_to_date <- function(process_row_df) {

  today <- lubridate::today(tzone = "Pacific/Auckland")
  
  dev_only_weekday <- extract_weekday_from_string(process_row_df$`Develop Only`)
  dev_and_scan_weekday <- extract_weekday_from_string(process_row_df$`Dev + Scan`)

  # E-6 dev only on turnaround times website says 'processed wednesday', but actually due the next day
  dev_only_days <- days_until_weekday(dev_only_weekday) + 1

  # For dev + scan, if the order comes in on wednesday, thursday or friday it won't be due until NEXT friday
  # This section handles if the order comes in on wednesday or thursday.
  # Since days_until_weekday() already excludes the current day, Friday handled implicitly
  today_wday <- weekdays(today)
  if (today_wday %in% c("wednesday", "thursday")) {
    # Adding 3 days to the date_from parameter to get us past friday, and into next week.
    # This gets us days until friday from saturday or sunday. Then add the extra 3 days so it gets us
    # days until friday next week from wednesday/thursday this week.
    dev_and_scan_days <- 3 + days_until_weekday(dev_and_scan_weekday, date_from = lubridate::today(tzone = "Pacific/Auckland") + 3)
  } else {
    # All other days can be handled like normal
    dev_and_scan_days <- days_until_weekday(dev_and_scan_weekday)
  }

  dev_only_date <- today + dev_only_days
  dev_and_scan_date <- today + dev_and_scan_days
  prints_add_on_date <- bizdays::offset(dev_and_scan_date, 1, "bawb_calendar") # 1 extra business day

  result_df <- tibble::tibble(
    "Service" = process_row_df$Service,
    "Develop Only" = dev_only_date,
    "Dev + Scan" = dev_and_scan_date,
    "Prints Add On" = prints_add_on_date
  )

  return(result_df)
}