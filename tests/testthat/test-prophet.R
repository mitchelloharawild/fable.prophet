context("test-prophet")
library(dplyr)

test_that("Prophet simple", {
  default <- model(tsibble::as_tsibble(USAccDeaths), prophet(value))
  expect_s3_class(default, "mdl_df")
  default_mdl <- default[[1]][[1]]$fit$model
  expect_length(default_mdl$seasonalities, 0)
  expect_length(default_mdl$changepoints, 25)
  expect_length(default_mdl$extra_regressors, 0)

  default_fc <- forecast(default, h = 17)
  expect_s3_class(default_fc, "fbl_ts")
  expect_equal(NROW(default_fc), 17)
})

test_that("Prophet complex", {
  vic_elec <- tsibbledata::aus_elec %>%
    filter(State == "Victoria", lubridate::year(Time) == 2014)
  elec_tr <- head(vic_elec, -48*31)
  elec_ts <- tail(vic_elec, 48*31)
  aus_holidays <- tsibble::as_tsibble(tsibble::holiday_aus(2014), index = date)
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
  expect_equal(NROW(complex_fc), 48*31)
})
