interval_to_period <- function(interval){
  with(interval, lubridate::years(year) +
         lubridate::period(3*quarter + month, units = "month") + lubridate::weeks(week) +
         lubridate::days(day) + lubridate::hours(hour) + lubridate::minutes(minute) +
         lubridate::seconds(second) + lubridate::milliseconds(millisecond) +
         lubridate::microseconds(microsecond) + lubridate::nanoseconds(nanosecond))
}
