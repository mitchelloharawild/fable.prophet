context("test-prophet")
library(dplyr)

test_that("Prophet simple", {
  default <- model(tsibble::as_tsibble(USAccDeaths), prophet(value ~ season("year")))
  expect_s3_class(default, "mdl_df")
  default_mdl <- default[[1]][[1]]$fit$model
  expect_length(default_mdl$seasonalities, 1)
  expect_length(default_mdl$changepoints, 25)
  expect_length(default_mdl$extra_regressors, 0)

  default_fc <- forecast(default, h = 17)
  expect_s3_class(default_fc, "fbl_ts")
  expect_equal(NROW(default_fc), 17)
})

test_that("Prophet complex", {
  skip_if_not_installed("tsibbledata")
  vic_elec <- tsibbledata::vic_elec %>%
    filter(lubridate::year(Time) == 2014)
  elec_tr <- vic_elec[1:(24*7*5),]
  elec_ts <- vic_elec[(24*7*5 + 1):(24*7*7),]
  aus_holidays <- tsibble::tsibble(
    holiday = c("New Year's Day", "Australia Day", "Good Friday",
                "Easter Monday", "ANZAC Day", "Christmas Day", "Boxing Day"),
    date = structure(c(16071, 16097, 16178, 16181, 16185, 16429, 16430), class = "Date"),
    index = date)
  complex <- model(elec_tr,
                   fit = prophet(Demand ~ growth('logistic', capacity = 10, floor = 2.5) +
                                   season("week", 3) + season("year", 12) + Temperature +
                                   holiday(aus_holidays))
  )

  expect_s3_class(complex, "mdl_df")
  complex_mdl <- complex[["fit"]][[1]]$fit$model
  expect_named(complex_mdl$seasonalities, c("week", "year"))
  expect_length(complex_mdl$changepoints, 25)
  expect_named(complex_mdl$extra_regressors, "Temperature")
  expect_equal(complex_mdl$holidays$holiday, aus_holidays$holiday)

  complex_fc <- forecast(complex, elec_ts)
  expect_s3_class(complex_fc, "fbl_ts")
  expect_equal(NROW(complex_fc), 24*7*2)
})
