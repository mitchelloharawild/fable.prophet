library(dplyr)

test_that("Prophet simple", {
  default <- model(tsibble::as_tsibble(USAccDeaths), prophet(value ~ season("year")))
  expect_true(is_mable(default))
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

  expect_true(is_mable(complex))
  complex_mdl <- complex[["fit"]][[1]]$fit$model
  expect_named(complex_mdl$seasonalities, c("week", "year"))
  expect_length(complex_mdl$changepoints, 25)
  expect_named(complex_mdl$extra_regressors, "Temperature")
  expect_equal(complex_mdl$holidays$holiday, aus_holidays$holiday)

  complex_fc <- forecast(complex, elec_ts)
  expect_s3_class(complex_fc, "fbl_ts")
  expect_equal(NROW(complex_fc), 24*7*2)
})

test_that("forecast ignores additional arguments", {
  fit <- model(tsibble::as_tsibble(USAccDeaths), prophet(value ~ season("year")))
  fc <- forecast(fit, h = 3, foo = 1)
  expect_s3_class(fc, "fbl_ts")
  expect_equal(NROW(fc), 3)
})

test_that("Prophet regressors with non-syntactic names", {
  set.seed(1)
  dat <- tsibble::tsibble(
    date = as.Date("2020-01-01") + 0:99,
    x = runif(100, 1, 5),
    index = date
  )
  dat$value <- 2 * log(dat$x) + rnorm(100, sd = 0.1)

  fit <- model(dat, prophet(value ~ log(x) + I(x^2)))
  mdl <- fit[[1]][[1]]$fit$model
  expect_equal(names(mdl$extra_regressors), c("log.x.", "I.x.2."))
  expect_equal(tidy(fit)$term[-(1:2)], c("log.x.", "I.x.2."))

  new_dat <- tsibble::new_data(dat, 5)
  new_dat$x <- runif(5, 1, 5)
  fc <- forecast(fit, new_data = new_dat)
  expect_s3_class(fc, "fbl_ts")
  expect_equal(NROW(fc), 5)
})

test_that("Prophet flat growth", {
  fit <- model(tsibble::as_tsibble(USAccDeaths), prophet(value ~ growth("flat") + season("year")))
  expect_equal(fit[[1]][[1]]$fit$model$growth, "flat")
  fc <- forecast(fit, h = 6)
  expect_s3_class(fc, "fbl_ts")
  expect_equal(NROW(fc), 6)
})

test_that("Prophet country holidays", {
  dat <- tsibble::tsibble(
    date = as.Date("2019-01-01") + 0:799,
    value = sin(0:799 / 30) + rnorm(800, sd = 0.1),
    index = date
  )
  fit <- model(dat, prophet(value ~ holiday(country = "AU")))
  mdl <- fit[[1]][[1]]$fit$model
  expect_equal(mdl$country_holidays, "AU")
  tdy <- tidy(fit)
  expect_equal(nrow(tdy), length(unlist(mdl$params[c("k", "m", "beta")])))
  expect_true(all(c("Christmas Day", "Anzac Day") %in% tdy$term))
  expect_false(anyNA(tdy$term))

  # Combined with a holiday table with windows
  hols <- tsibble::tsibble(
    holiday = "Party", date = as.Date(c("2019-06-01", "2020-06-01")),
    lower_window = -1, upper_window = 1, index = date
  )
  fit2 <- model(dat, prophet(value ~ holiday(hols, country = "AU")))
  tdy2 <- tidy(fit2)
  expect_equal(nrow(tdy2), length(unlist(fit2[[1]][[1]]$fit$model$params[c("k", "m", "beta")])))
  expect_true(all(c("Party_-1", "Party", "Party_+1") %in% tdy2$term))

  fc <- forecast(fit, h = 5)
  expect_equal(NROW(fc), 5)
})

test_that("Prophet conditional seasonality", {
  dat <- tsibble::tsibble(
    date = as.Date("2020-01-01") + 0:199,
    index = date
  )
  dat$on <- as.numeric(format(dat$date, "%m")) <= 4
  dat$value <- ifelse(dat$on, sin(2 * pi * (0:199) / 7), 0) + rnorm(200, sd = 0.1)

  fit <- model(dat, prophet(value ~ season(7, 3, name = "cond_week", condition = on)))
  mdl <- fit[[1]][[1]]$fit$model
  expect_equal(mdl$seasonalities$cond_week$condition.name, "on")

  new_dat <- tsibble::new_data(dat, 7)
  new_dat$on <- c(TRUE, FALSE, TRUE, TRUE, FALSE, FALSE, TRUE)
  fc <- forecast(fit, new_data = new_dat)
  expect_s3_class(fc, "fbl_ts")
  expect_equal(NROW(fc), 7)

  # Missing from new_data
  expect_error(forecast(fit, h = 3), "conditional seasonality column `on`")
  # Not a column of the data
  expect_warning(
    model(dat, prophet(value ~ season(7, 3, name = "cw", condition = nope))),
    "conditional seasonality column `nope`"
  )
})

test_that("tidy reports regressor coefficients on the original scale", {
  set.seed(1)
  dat <- tsibble::tsibble(
    date = as.Date("2019-01-01") + 0:299,
    x = runif(300, 0, 10),
    index = date
  )
  dat$value <- 100 + 3 * dat$x + rnorm(300)
  fit <- model(dat, prophet(value ~ x + growth(n_changepoints = 5)))
  tdy <- tidy(fit)
  expect_equal(tdy$term[nrow(tdy)], "x")
  expect_equal(tdy$estimate[nrow(tdy)], 3, tolerance = 0.1)
  mdl <- fit[[1]][[1]]$fit$model
  expect_equal(
    tdy$estimate[nrow(tdy)],
    c(mdl$params$beta) * mdl$y.scale / mdl$extra_regressors$x$std
  )
})

test_that("components include holiday and regressor terms", {
  set.seed(1)
  dat <- tsibble::tsibble(
    date = as.Date("2019-01-01") + 0:499,
    x = runif(500, 0, 10),
    z = rbinom(500, 1, 0.5),
    index = date
  )
  dat$value <- 100 + 3 * dat$x + sin(0:499 / 20) + rnorm(500)
  fit <- model(dat, prophet(value ~ x + xreg(z, type = "multiplicative") + holiday(country = "AU")))
  cmp <- components(fit)
  expect_true(all(
    c("holidays", "extra_regressors_additive", "extra_regressors_multiplicative") %in% colnames(cmp)
  ))
  expect_equal(
    cmp$trend * (1 + cmp$multiplicative_terms) + cmp$additive_terms + cmp$.resid,
    cmp$value
  )
  expect_equal(cmp$additive_terms, cmp$holidays + cmp$extra_regressors_additive)
  expect_equal(cmp$multiplicative_terms, cmp$extra_regressors_multiplicative)
  expect_s3_class(cmp, "dcmp_ts")

  # Models without them are unchanged
  cmp0 <- components(model(dat, prophet(value ~ season("year"))))
  expect_false(any(c("holidays", "extra_regressors_additive") %in% colnames(cmp0)))
})

test_that("forecast with times = 0 gives point forecasts", {
  fit <- model(tsibble::as_tsibble(USAccDeaths), prophet(value ~ season("year")))
  fc <- forecast(fit, h = 6, times = 0)
  expect_s3_class(fc, "fbl_ts")
  expect_equal(NROW(fc), 6)
  expect_true(distributional::is_distribution(fc$value))
  expect_true(all(distributional::variance(fc$value) == 0))
  expect_false(anyNA(mean(fc$value)))

  # Close to the mean of simulated paths
  fc_sim <- forecast(fit, h = 6, times = 500)
  expect_equal(mean(fc$value), mean(fc_sim$value), tolerance = 0.1)
})
