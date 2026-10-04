# fable.prophet (development version)

* `season()` gains a `condition` argument (a bare logical column name) for conditional seasonality (via `prophet::add_seasonality(condition.name = )`). The column must also be present in `new_data` when forecasting.
* `holiday()` gains a `country` argument to include prophet's built-in country holidays (via `prophet::add_country_holidays()`), with or without a `holidays` table. `tidy()` now names (and orders) holiday terms from the fitted model, so country holidays and repeated holiday dates are supported.
* `growth()` now supports `type = "flat"`, for a constant trend without changepoints.
* Regressors with non-syntactic names (e.g. `log(x)` or `I(x^2)`) are now supported, using `make.names()` to name them in the model and in `tidy()` (#32).
* Removed an unused prediction in `forecast()` (#33).
* `forecast()` no longer forwards `...` to `prophet::predictive_samples()`, which doesn't accept additional arguments. `...` is now documented as unused.

# fable.prophet 0.1.1

Small patch for compatibility with fabletools v1.0.0.

* Fixed error with non-syntactically valid index variable names.
* Updated broken and moved URLs in the README and vignette.

# fable.prophet 0.1.0

* First release.

## New features

* Added interface for the Prophet model (via the 'prophet' R package) to the fable framework.
* Added prophet model methods for: `forecast()`, `components()`, `fitted()`, `residuals()`.
* Added package introduction vignette.
