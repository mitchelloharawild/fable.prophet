## code to prepare `lax_passengers` dataset goes here

library(tsibble)
library(dplyr)
library(lubridate)
lax_passengers <- as_tibble(read.csv("data-raw/lax_passengers.csv")) %>%
  mutate(datetime = mdy_hms(ReportPeriod)) %>%
  group_by(date = as_date(datetime), type = Domestic_International) %>%
  summarise(passengers = sum(Passenger_Count)) %>%
  ungroup() %>%
  as_tsibble(index = date, key = type)

# usethis::use_data("lax_passengers")
