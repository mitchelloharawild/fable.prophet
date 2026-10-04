# fable.prophet (development version)

* Documentation: added "Non-daily data" (#28), "Performance and memory" (#26) and model evaluation sections to `?prophet` and the introduction vignette.
* `prophet()` now documents and supports the `mcmc.samples` and `backend` arguments (passed to `prophet::prophet()`) for MCMC estimation and choosing the `"rstan"` or `"cmdstanr"` Stan backend. `tidy()` and `glance()` summarise MCMC draws by their posterior means.
* The model is now fit with `uncertainty.samples = 0` (previously the argument was misspelt and ignored).
* Fitted models no longer keep prophet's raw Stan output (`stan.fit`), reducing model size (#26).
* `forecast()` now supports `times = 0`, which returns point forecasts (as degenerate distributions) without simulating sample paths. The `times` argument is now documented, including its speed and memory trade-off (#26).
* `components()` now includes the `holidays`, `extra_regressors_additive` and `extra_regressors_multiplicative` terms when they are present in the model.
* `tidy()` now reports extra regressor estimates as coefficients on the original data scale (additive regressors in units of the response, multiplicative regressors as proportional effects), rather than prophet's internal scaled parameters. Other terms remain on the internal scale, as documented in `?tidy.fbl_prophet`.
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
