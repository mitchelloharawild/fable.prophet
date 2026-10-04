#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables("self")

#' @importFrom stats predict
train_prophet <- function(.data, specials, mcmc.samples = 0, backend = NULL, ...){
  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by Prophet")
  }

  # The training arguments are kept on the fit, so that refit() can reuse them
  args <- list(
    auto_seasonality = is.name(self$formula),
    mcmc.samples = mcmc.samples, backend = backend, fit_args = list(...)
  )
  fit_prophet(.data, specials, args)
}

# Specify and estimate a prophet model (shared by training and refitting)
fit_prophet <- function(.data, specials, args){
  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by Prophet")
  }

  # Prepare data for modelling
  model_data <- as_tibble(.data)[c(index_var(.data), measured_vars(.data))]
  colnames(model_data) <- c("ds", "y")

  # Growth
  growth <- specials$growth[[1]]

  # Holidays
  holiday <- specials$holiday[[1]]

  # Build model
  mdl <- prophet::prophet(
    growth = growth$type,
    changepoints = growth$changepoints,
    n.changepoints = growth$n_changepoints,
    changepoint.range = growth$changepoint_range,
    changepoint.prior.scale = growth$changepoint_prior_scale,
    holidays = holiday$holidays,
    holidays.prior.scale = holiday$prior_scale,
    yearly.seasonality = args$auto_seasonality,
    weekly.seasonality = args$auto_seasonality,
    daily.seasonality = args$auto_seasonality,
    mcmc.samples = args$mcmc.samples,
    uncertainty.samples = 0,
    backend = args$backend
  )

  if(!is.null(holiday$country)){
    mdl <- prophet::add_country_holidays(mdl, holiday$country)
  }

  # Seasonality
  for (season in specials$season){
    mdl <- prophet::add_seasonality(
      mdl, name = season$name, period = season$period,
      fourier.order = season$order, prior.scale = season$prior_scale,
      mode = season$type, condition.name = season$condition_name)
  }

  # Exogenous Regressors
  xreg_names <- xreg_safe_names(specials$xreg)
  i <- 0L
  for(regressor in specials$xreg){
    for(j in seq_len(ncol(regressor$xreg))){
      i <- i + 1L
      mdl <- prophet::add_regressor(
        mdl, name = xreg_names[[i]], prior.scale = regressor$prior_scale,
        standardize = regressor$standardize, mode = regressor$mode)
    }
  }

  # Predictors (growth, conditional seasonality and regressors)
  model_data <- prophet_predictors(model_data, specials)

  # Train model
  mdl <- do.call(prophet::fit.prophet, c(list(mdl, model_data), args$fit_args))
  mdl$uncertainty.samples <- 0
  # The raw Stan output is not used after fitting (parameters are in `$params`)
  mdl$stan.fit <- NULL

  structure(
    c(list(model = mdl), prophet_fitted(mdl, model_data, .data), list(args = args)),
    class = "fbl_prophet")
}

# Add the prophet predictor columns (carrying capacity and floor, conditional
# seasonality conditions and regressors) from the specials to a data frame
# containing the time index as `ds`.
prophet_predictors <- function(data, specials){
  ## Growth
  growth <- specials$growth[[1]]
  if(!is.null(growth$capacity)){
    data$cap <- growth$capacity
  }
  if(!is.null(growth$floor)){
    data$floor <- growth$floor
  }

  ## Conditional seasonality
  for(season in specials$season){
    if(!is.null(season$condition_name)){
      data[[season$condition_name]] <- season$condition_values
    }
  }

  ## Exogenous Regressors
  xreg_names <- xreg_safe_names(specials$xreg)
  i <- 0L
  for(regressor in specials$xreg){
    for(j in seq_len(ncol(regressor$xreg))){
      i <- i + 1L
      data[xreg_names[[i]]] <- regressor$xreg[,j]
    }
  }
  data
}

# Fitted values, residuals and components of a fitted prophet model over the
# data it was (or is to be) evaluated on. `model_data` has the `ds` and `y`
# columns and predictors, and `.data` is the tsibble the components are for.
prophet_fitted <- function(mdl, model_data, .data){
  fits <- predict(mdl, model_data)

  # Components to decompose: holiday and regressor terms exist when in the model
  cmp_names <- intersect(
    c("holidays", "extra_regressors_additive", "extra_regressors_multiplicative"),
    names(fits)
  )

  list(
    est = list(.fitted = fits$yhat, .resid = model_data[["y"]] - fits$yhat),
    components = .data %>% mutate(!!!(fits[c("additive_terms", "multiplicative_terms", "trend", names(mdl$seasonalities), cmp_names)]))
  )
}

# Prophet requires syntactically valid regressor names (e.g. `log(x)` is not).
# Applied jointly over all xreg specials so train and forecast agree.
xreg_safe_names <- function(xreg){
  make.names(unlist(lapply(xreg, function(x) colnames(x$xreg))), unique = TRUE)
}

# Names of the holiday terms (one per holiday feature, in the order prophet
# stores their coefficients). Includes holidays from `add_country_holidays()`.
holiday_term_names <- function(mdl){
  hols <- mdl$holidays
  nms <- unique(c(hols$holiday, mdl$train.holiday.names))
  if(length(nms) == 0) return(NULL)
  keys <- character()
  labs <- character()
  for(nm in nms){
    h <- hols[hols$holiday == nm, , drop = FALSE]
    lower <- if(is.null(h$lower_window) || all(is.na(h$lower_window))) 0 else h$lower_window[!is.na(h$lower_window)][1]
    upper <- if(is.null(h$upper_window) || all(is.na(h$upper_window))) 0 else h$upper_window[!is.na(h$upper_window)][1]
    offsets <- seq(lower, upper)
    keys <- c(keys, paste0(nm, "_delim_", ifelse(offsets < 0, "-", "+"), abs(offsets)))
    labs <- c(labs, paste0(nm, ifelse(offsets > 0, paste0("_+", offsets), ifelse(offsets < 0, paste0("_", offsets), ""))))
  }
  # Prophet sorts holiday features by name
  labs[order(keys)]
}

specials_prophet <- new_specials(
  growth = function(type = c("linear", "logistic", "flat"),
                   capacity = NULL, floor = NULL,
                   changepoints = NULL, n_changepoints = 25,
                   changepoint_range = 0.8, changepoint_prior_scale = 0.05){
    capacity <- eval_tidy(enquo(capacity), data = self$data)
    floor <- eval_tidy(enquo(floor), data = self$data)
    type <- match.arg(type)
    as.list(environment())
  },
  season = function(period = NULL, order = NULL, prior_scale = 10,
                    type = c("additive", "multiplicative"),
                    name = NULL, condition = NULL){
    # Conditional seasonality column (a bare column name)
    condition_name <- NULL
    condition_values <- NULL
    if(!is.null(enexpr(condition))){
      condition_expr <- enexpr(condition)
      if(!is.name(condition_expr)){
        abort("The `condition` of `season()` must be a bare column name of the data.")
      }
      condition_name <- as_string(condition_expr)
      if(!(condition_name %in% colnames(self$data))){
        abort(sprintf(
          "The conditional seasonality column `%s` was not found in the data. It must be a logical column in both the training data and `new_data`.",
          condition_name))
      }
      condition_values <- self$data[[condition_name]]
      if(!is.logical(condition_values) || anyNA(condition_values)){
        abort(sprintf("The conditional seasonality column `%s` must be logical without missing values.", condition_name))
      }
    }

    # Extract data interval
    interval <- tsibble::interval(self$data)
    interval <- interval_to_period(interval)

    if(is.null(name) & is.character(period)){
      name <- period
    }

    # Compute prophet interval
    period <- get_frequencies(period, self$data, .auto = "smallest")
    period <- period * suppressMessages(interval/lubridate::days(1))

    if(is.null(name)){
      name <- paste0("season", period)
    }

    if(is.null(order)){
      if(period %in% c(365.25, 7, 1)){
        order <- c(10, 3, 4)[period == c(365.25, 7, 1)]
      }
      else{
        abort(
          sprintf("Unable to add %s to the model. The fourier order has no default, and must be specified with `order = ?`.",
                  deparse(match.call()))
        )
      }
    }
    order <- as.integer(order)
    type <- match.arg(type)
    as.list(environment())
  },
  holiday = function(holidays = NULL, prior_scale = 10L, country = NULL){
    if(tsibble::is_tsibble(holidays)){
      holidays <- rename(as_tibble(holidays), ds = !!index(holidays))
    }
    as.list(environment())
  },
  xreg = function(..., prior_scale = NULL, standardize = "auto", type = NULL){
    model_formula <- new_formula(
      lhs = NULL,
      rhs = reduce(c(0, enexprs(...)), function(.x, .y) call2("+", .x, .y))
    )
    list(
      xreg = model.matrix(model_formula, self$data),
      prior_scale = prior_scale,
      standardize = standardize,
      mode = type
    )
  },
  .required_specials = c("growth", "holiday")
)

#' Prophet procedure modelling
#'
#' Prepares a prophet model specification for use within the `fable` package.
#'
#' The prophet modelling interface uses a `formula` based model specification
#' (`y ~ x`), where the left of the formula specifies the response variable,
#' and the right specifies the model's predictive terms. Like any model in the
#' fable framework, it is possible to specify transformations on the response.
#'
#' A prophet model supports piecewise linear or exponential growth (trend),
#' additive or multiplicative seasonality, holiday effects and exogenous
#' regressors. These can be specified using the 'specials' functions detailed
#' below. The introduction vignette provides more details on how to model data
#' using this interface to prophet: `vignette("intro", package="fable.prophet")`.
#'
#' @param formula A symbolic description of the model to be fitted of class `formula`.
#' @param ... Additional arguments for estimating the model. These are
#'   `mcmc.samples` and `backend` (described below), with any others passed on to
#'   [`prophet::fit.prophet()`] and then to the Stan algorithm (for example
#'   `algorithm`, `iter` or `init` for optimisation, or `control` when
#'   `mcmc.samples > 0`), for example
#'   `prophet(y ~ season("year"), algorithm = "Newton")`.
#'
#' @section Estimation:
#' By default the model is estimated by maximum a posteriori (MAP) optimisation.
#' Two arguments of [`prophet::prophet()`] can be given in `...` of `prophet()`:
#' \itemize{
#'   \item `mcmc.samples`: If greater than 0, the model is estimated using this
#'   many MCMC (Hamiltonian Monte Carlo) iterations, which is slower. Parameters
#'   are then summarised by their posterior means in [`tidy()`][tidy.fbl_prophet()]
#'   and [`glance()`][glance.fbl_prophet()], and forecast sample paths include
#'   parameter uncertainty.
#'   \item `backend`: The Stan backend, either `"rstan"` or `"cmdstanr"` (which
#'   requires the \pkg{cmdstanr} package). If `NULL` (the default), the backend
#'   is chosen by [`prophet::prophet()`], using `"rstan"` unless the
#'   environment variable `R_STAN_BACKEND` is set to `"CMDSTANR"`.
#' }
#'
#' @section Specials:
#'
#' \subsection{growth}{
#' The `growth` special is used to specify the trend parameters.
#' \preformatted{
#' growth(type = c("linear", "logistic", "flat"), capacity = NULL, floor = NULL,
#'        changepoints = NULL, n_changepoints = 25, changepoint_range = 0.8,
#'        changepoint_prior_scale = 0.05)
#' }
#'
#' \tabular{ll}{
#'   `type`                    \tab The type of trend (linear, logistic or flat). A flat trend is a constant level with no changepoints.\cr
#'   `capacity`                \tab The carrying capacity for when `type` is "logistic".\cr
#'   `floor`                   \tab The saturating minimum for when `type` is "logistic".\cr
#'   `changepoints`            \tab A vector of dates/times for changepoints. If `NULL`, changepoints are automatically selected.\cr
#'   `n_changepoints`          \tab The total number of changepoints to be selected if `changepoints` is `NULL`\cr
#'   `changepoint_range`       \tab Proportion of the start of the time series where changepoints are automatically selected.\cr
#'   `changepoint_prior_scale` \tab Controls the flexibility of the trend.
#' }
#' }
#'
#' \subsection{season}{
#' The `season` special is used to specify a seasonal component. This special can be used multiple times for different seasonalities.
#'
#' **Warning: The inputs controlling the seasonal `period` is specified is different than [`prophet::prophet()`]. Numeric inputs are treated as the number of observations in each seasonal period, not the number of days.**
#'
#' \preformatted{
#' season(period = NULL, order = NULL, prior_scale = 10,
#'        type = c("additive", "multiplicative"), name = NULL, condition = NULL)
#' }
#'
#' \tabular{ll}{
#'   `period`      \tab The periodic nature of the seasonality. If a number is given, it will specify the number of observations in each seasonal period. If a character is given, it will be parsed using `lubridate::as.period`, allowing seasonal periods such as "2 years".\cr
#'   `order`       \tab The number of terms in the partial Fourier sum. The higher the `order`, the more flexible the seasonality can be.\cr
#'   `prior_scale` \tab Used to control the amount of regularisation applied. Reducing this will dampen the seasonal effect.\cr
#'   `type`        \tab The nature of the seasonality. If "additive", the variability in the seasonal pattern is fixed. If "multiplicative", the seasonal pattern varies proportionally to the level of the series.\cr
#'   `name`        \tab The name of the seasonal term (allowing you to name an annual pattern as 'annual' instead of 'year' or `365.25` for example).\cr
#'   `condition`   \tab A bare column name of a logical variable, for conditional seasonality (see [`prophet::add_seasonality()`]). The seasonality is only applied when the condition is `TRUE`, and the column must be present (without missing values) in the data and in `new_data` when forecasting.\cr
#' }
#' }
#'
#' \subsection{holiday}{
#' The `holiday` special is used to specify a `tsibble` containing holidays for the model, and/or a country whose built-in holidays are included.
#' \preformatted{
#' holiday(holidays = NULL, prior_scale = 10L, country = NULL)
#' }
#'
#' \tabular{ll}{
#'   `holidays`    \tab A [`tsibble`](https://tsibble.tidyverts.org/) containing a set of holiday events. The event name is given in the 'holiday' column, and the event date is given via the index. Additionally, "lower_window" and "upper_window" columns can be used to include days before and after the holiday.\cr
#'   `prior_scale` \tab Used to control the amount of regularisation applied. Reducing this will dampen the holiday effect.\cr
#'   `country`     \tab A country name or code (e.g. "AU") for which to include prophet's built-in holidays, see [`prophet::add_country_holidays()`]. Can be used with or without `holidays`.\cr
#' }
#' }
#'
#' \subsection{xreg}{
#' The `xreg` special is used to include exogenous regressors in the model. This special can be used multiple times for different regressors with different arguments.
#' Exogenous regressors can also be used in the formula without explicitly using the `xreg()` special, which will then use the default arguments.
#' \preformatted{
#' xreg(..., prior_scale = NULL, standardize = "auto", type = NULL)
#' }
#'
#' \tabular{ll}{
#'   `...`         \tab A set of bare expressions that are evaluated as exogenous regressors\cr
#'   `prior_scale` \tab Used to control the amount of regularisation applied. Reducing this will dampen the regressor effect.\cr
#'   `standardize` \tab Should the regressor be standardised before fitting? If "auto", it will standardise if the regressor is not binary.\cr
#'   `type`        \tab Does the effect of the regressor vary proportionally to the level of the series? If so, "multiplicative" is best. Otherwise, use "additive"\cr
#' }
#' }
#'
#' @section Non-daily data:
#' A model without any terms (`prophet(y)`, a bare response with no right hand
#' side) enables prophet's yearly, weekly and daily seasonalities, as in
#' [`prophet::prophet()`]. Unlike prophet's default (`"auto"`), these are
#' switched on regardless of the data's frequency or length, so for monthly or
#' quarterly data (or any data coarser than daily) the weekly and daily
#' seasonal terms are not meaningful and `prophet(y)` is not recommended.
#' Instead, specify the seasonality explicitly with `season()`. Any formula
#' (such as `prophet(y ~ season(...))`) uses only the seasonalities that you
#' specify, with no automatic seasonalities. For example, use
#' `prophet(y ~ season(period = "year", order = 4))` for quarterly data, or
#' `order = 6` for monthly data. A `season()` period given as a string is
#' converted using the data's interval, and a number is the count of
#' observations per period. A Fourier `order` is required for periods other
#' than a year, week or day.
#'
#' @section Performance and memory:
#' Fitting prophet models is relatively slow, and each forecast stores
#' `times` simulated sample paths (see [`forecast.fbl_prophet()`]). When
#' modelling many series, consider the following:
#' \itemize{
#'   \item Reduce `times` in `forecast()` (for example `times = 100`), or use
#'   `times = 0` for point forecasts only. This reduces time and the size of the
#'   forecast object.
#'   \item Fitted models drop the raw Stan output, but keep the data used to fit
#'   the model, so very large mables can be made smaller by
#'   forecasting then discarding the models.
#'   \item Models are independent, so series can be fit and forecast in
#'   parallel using the \pkg{future} package, for example with
#'   `future::plan(future::multisession)`.
#'   \item MCMC estimation (`mcmc.samples`) is much slower than the default
#'   optimisation.
#' }
#'
#' @section Model evaluation:
#' Prophet's `cross_validation()` and `performance_metrics()` are not needed
#' for this interface. Use [`tsibble::stretch_tsibble()`] to create
#' cross-validation folds, fit models on them and
#' [`fabletools::accuracy()`] to evaluate the forecasts.
#'
#' @seealso
#' - [`prophet::prophet()`]
#' - [Prophet homepage](https://facebook.github.io/prophet/)
#' - [Prophet R package](https://CRAN.R-project.org/package=prophet)
#' - [Prophet Python package](https://pypi.org/project/fbprophet/)
#'
#' @examples
#' library(tsibble)
#' as_tsibble(USAccDeaths) %>%
#'   model(
#'     prophet = prophet(value ~ season("year", 4, type = "multiplicative"))
#'   )
#'
#' @export
prophet <- function(formula, ...){
  prophet_model <- new_model_class("prophet", train_prophet, specials_prophet)
  new_model_definition(prophet_model, !!enquo(formula), ...)
}

#' Produce forecasts from the prophet model
#'
#' If additional future information is required (such as exogenous variables or
#' carrying capacities) by the model, then they should be included as variables
#' of the `new_data` argument.
#'
#' @inheritParams fable::forecast.ARIMA
#' @param times The number of sample paths simulated from the model's predictive
#'   distribution (default 1000). Each forecast is a sample distribution
#'   ([`distributional::dist_sample()`]) of this many paths, so `times` directly
#'   controls the time and memory used when forecasting many series or horizons
#'   (for example, with parallel workers via the \pkg{future} package).
#'   Reducing `times` (e.g. `times = 100`) speeds up forecasting and reduces the
#'   size of the forecast object, at the cost of noisier intervals. If
#'   `times = 0`, no paths are simulated and a point forecast is returned as a
#'   degenerate distribution ([`distributional::dist_degenerate()`]) of the
#'   model's predicted values, so prediction intervals are not available.
#' @param ... Currently unused and ignored.
#'
#' @seealso [`prophet::predict.prophet()`]
#'
#' @return A list of forecasts.
#'
#' @examples
#'
#' \donttest{
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' tsibbledata::aus_production %>%
#'   model(
#'     prophet = prophet(Beer ~ season("year", 4, type = "multiplicative"))
#'   ) %>%
#'   forecast()
#' }
#' }
#'
#' @export
forecast.fbl_prophet <- function(object, new_data, specials = NULL, times = 1000, ...){
  mdl <- object$model

  # Prepare data
  new_data <- rename(as.data.frame(new_data), ds = !!index(new_data))
  new_data <- prophet_predictors(new_data, specials)

  # Point forecasts without simulation
  if(times == 0){
    mdl$uncertainty.samples <- 0
    return(distributional::dist_degenerate(predict(mdl, new_data)$yhat))
  }

  # Simulate future paths
  mdl$uncertainty.samples <- times
  sim <- prophet::predictive_samples(mdl, new_data)$yhat
  sim <- split(sim, row(sim))

  # Return forecasts
  distributional::dist_sample(sim)
}

#' Refit a prophet model
#'
#' Applies a prophet model to a new dataset. By default (`reestimate = FALSE`)
#' the estimated parameters (including the trend changepoints and the scaling
#' of the data) are kept, and only the fitted values, residuals and components
#' are recomputed for `new_data`. If `reestimate = TRUE`, the model is instead
#' estimated again on `new_data`, using the same specification and estimation
#' arguments (such as `mcmc.samples`, `backend` and any others given to
#' [`prophet()`]) as the original model.
#'
#' Prophet does not support updating a fitted model with new observations, so
#' re-estimating is a fresh fit (the previous parameters are not used to
#' initialise it), and no `stream()` method is defined.
#'
#' With `reestimate = FALSE`, the data should span the time period that the
#' model is to be evaluated over, and any variables required by the model
#' (regressors, carrying capacities and conditions) must be in `new_data`.
#'
#' @inheritParams fable::refit.ARIMA
#' @param ... Currently unused and ignored.
#'
#' @return A refitted model.
#'
#' @examples
#' library(tsibble)
#' fit <- as_tsibble(USAccDeaths) %>%
#'   dplyr::filter(index < yearmonth("1977 Jan")) %>%
#'   model(prophet(value ~ season("year", 4)))
#'
#' # Evaluate the estimated model on the full series
#' refit(fit, as_tsibble(USAccDeaths))
#'
#' # Estimate the model again on the full series
#' refit(fit, as_tsibble(USAccDeaths), reestimate = TRUE)
#'
#' @export
refit.fbl_prophet <- function(object, new_data, specials = NULL, reestimate = FALSE, ...){
  if(reestimate){
    return(fit_prophet(new_data, specials, object$args))
  }

  mdl <- object$model
  model_data <- as_tibble(new_data)[c(index_var(new_data), measured_vars(new_data))]
  colnames(model_data) <- c("ds", "y")
  model_data <- prophet_predictors(model_data, specials)

  structure(
    c(list(model = mdl), prophet_fitted(mdl, model_data, new_data), list(args = object$args)),
    class = "fbl_prophet")
}

#' Generate responses from a prophet model
#'
#' Simulates future (or in-sample) sample paths from a prophet model. Each path
#' consists of a simulated trend (including future trend changes), the model's
#' seasonal, holiday and regressor effects, and observation noise.
#'
#' By default the observation noise is normally distributed with the estimated
#' standard deviation of the model (`sigma_obs`). If `bootstrap = TRUE`, the
#' noise is instead resampled from the model's (mean-centred) residuals.
#' When used via [`fabletools::generate()`], the `bootstrap` argument of that
#' function takes precedence and `times` sets the number of paths.
#'
#' As with [`forecast.fbl_prophet()`], any variables required by the model
#' (regressors, carrying capacities and conditions) must be included in
#' `new_data`. Estimated models using MCMC simulate each path using one
#' (randomly selected) posterior draw of the parameters.
#'
#' @inheritParams fable::generate.ARIMA
#' @param x A fitted prophet model.
#' @param new_data The data to simulate over. When used via
#'   [`fabletools::generate()`] it contains a `.rep` key for the replications,
#'   which are all simulated over the same time points.
#' @param ... Currently unused and ignored.
#'
#' @return A tsibble with the index and key of `new_data`, and the simulated
#'   values in `.sim`.
#'
#' @examples
#' library(tsibble)
#' fit <- as_tsibble(USAccDeaths) %>%
#'   model(prophet(value ~ season("year", 4)))
#'
#' generate(fit, h = 12, times = 5)
#'
#' @export
generate.fbl_prophet <- function(x, new_data = NULL, bootstrap = FALSE, specials = NULL, ...){
  if(is.null(new_data)){
    abort("`new_data` is required to generate responses from a prophet model.")
  }
  mdl <- x$model
  idx <- index_var(new_data)
  rows <- lapply(key_data(new_data)[[".rows"]], as.integer)
  df <- rename(as.data.frame(new_data), ds = !!idx)
  df <- prophet_predictors(df, specials)

  # Observation noise, either supplied, bootstrapped or normally distributed
  innov <- df[[".innov"]]
  if(is.null(innov) && bootstrap){
    res <- stats::na.omit(x$est$.resid)
    innov <- sample(res - mean(res), nrow(df), replace = TRUE)
  }

  # All replications share the same data when generated by fabletools
  keys <- setdiff(names(df), c(key_vars(new_data), ".innov"))
  first <- df[rows[[1]], keys, drop = FALSE]
  rownames(first) <- NULL
  shared <- all(lengths(rows) == length(rows[[1]])) && all(vapply(rows, function(r){
    d <- df[r, keys, drop = FALSE]
    rownames(d) <- NULL
    identical(d, first)
  }, logical(1L)))
  groups <- if(shared) list(seq_along(rows)) else as.list(seq_along(rows))

  sim <- numeric(nrow(df))
  for(g in groups){
    paths <- prophet_sample_paths(mdl, df[rows[[g[1]]], , drop = FALSE], length(g))
    for(k in seq_along(g)){
      r <- rows[[g[k]]]
      sim[r] <- paths$paths[, k]
      sim[r] <- sim[r] + if(is.null(innov)) stats::rnorm(length(r), sd = paths$sd[k]) else innov[r]
    }
  }

  transmute(new_data, .sim = sim)
}

# Simulate `times` sample paths of a prophet model without observation noise.
# Returns the paths (a matrix with a row for each row of `df`) and the standard
# deviation of the observation noise of the draw that each path used.
prophet_sample_paths <- function(mdl, df, times){
  # Remove observation noise from prophet's simulation (added separately)
  sigma <- mdl$params$sigma_obs
  mdl$params$sigma_obs[] <- 0
  n_iter <- length(mdl$params$k)
  mdl$uncertainty.samples <- times
  paths <- prophet::predictive_samples(mdl, df)$yhat

  # Paths are made for each iteration, in blocks of equal size
  per_iter <- max(1, ceiling(times / n_iter))
  use <- if(ncol(paths) > times) sort(sample.int(ncol(paths), times)) else seq_len(ncol(paths))
  list(
    paths = paths[, use, drop = FALSE],
    sd = sigma[ceiling(use / per_iter)] * mdl$y.scale
  )
}

#' Extract fitted values
#'
#' Extracts the fitted values from an estimated Prophet model.
#'
#' @inheritParams fable::fitted.ARIMA
#'
#' @return A vector of fitted values.
#'
#' @export
fitted.fbl_prophet <- function(object, ...){
  object$est[[".fitted"]]
}

#' Extract model residuals
#'
#' Extracts the residuals from an estimated Prophet model.
#'
#' @inheritParams fable::residuals.ARIMA
#'
#' @return A vector of residuals.
#'
#' @export
residuals.fbl_prophet <- function(object, ...){
  object$est[[".resid"]]
}

#' Extract meaningful components
#'
#' A prophet model consists of terms which are additively or multiplicatively
#' included in the model. Multiplicative terms are scaled proportionally to the
#' estimated trend, while additive terms are not.
#'
#' Holiday effects (`holidays`) and exogenous regressor effects
#' (`extra_regressors_additive` and `extra_regressors_multiplicative`) are
#' included when they are part of the model. Like the seasonal terms, these are
#' contained within the model's `additive_terms` or `multiplicative_terms`.
#'
#' Extracting a prophet model's components using this function allows you to
#' visualise the components in a similar way to [`prophet::prophet_plot_components()`].
#'
#' @inheritParams fable::components.ETS
#'
#' @return A [`fabletools::dable()`] containing estimated states.
#'
#' @examples
#'
#' \donttest{
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' beer_components <- tsibbledata::aus_production %>%
#'   model(
#'     prophet = prophet(Beer ~ season("year", 4, type = "multiplicative"))
#'   ) %>%
#'   components()
#'
#' beer_components
#'
#' autoplot(beer_components)
#'
#' library(ggplot2)
#' library(lubridate)
#' beer_components %>%
#'   ggplot(aes(x = quarter(Quarter), y = year, group = year(Quarter))) +
#'   geom_line()
#' }
#' }
#'
#' @export
components.fbl_prophet <- function(object, ...){
  cmp <- object$components
  cmp$.resid <- object$est$.resid
  mv <- measured_vars(cmp)
  as_dable(cmp, resp = !!sym(mv[1]), method = "Prophet",
           aliases = set_names(
             list(expr(!!sym("trend") * (1 + !!sym("multiplicative_terms")) + !!sym("additive_terms") + !!sym(".resid"))),
             mv[1]
           )
  )
}

#' Glance a prophet model
#'
#' A glance of a prophet provides the residual's standard deviation (sigma), and
#' a tibble containing the selected changepoints with their trend adjustments.
#'
#' @inheritParams fable::glance.ARIMA
#'
#' @return A one row tibble summarising the model's fit.
#'
#' @examples
#'
#' \donttest{
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' library(dplyr)
#' fit <- tsibbledata::aus_production %>%
#'   model(
#'     prophet = prophet(Beer ~ season("year", 4, type = "multiplicative"))
#'   )
#'
#' glance(fit)
#' }
#' }
#'
#' @export
glance.fbl_prophet <- function(x, ...){
  changepoints <- tibble(
    changepoints = x$model$changepoints,
    adjustment = colMeans(x$model$params$delta)
  )
  tibble(sigma = stats::sd(x$est$.resid, na.rm = TRUE), changepoints = list(changepoints))
}

#' Extract estimated coefficients from a prophet model
#'
#' @inheritParams fable::tidy.ARIMA
#'
#' @details
#' The `estimate` of the growth (`base_growth`, `trend_offset`), seasonal and
#' holiday terms are the model's parameters on prophet's internal (scaled)
#' scale. For extra regressors, the estimate is instead the coefficient on the
#' scale of the original data: additive regressors are reported in units of the
#' response per unit of the regressor, and multiplicative regressors as the
#' proportional change in the trend per unit of the regressor. This is the
#' same quantity described by [`prophet::regressor_coefficients()`] (whose
#' returned `coef`, in prophet 1.1.7, is not yet rescaled).
#'
#' @return A tibble containing the model's estimated parameters.
#'
#' @examples
#'
#' \donttest{
#' if (requireNamespace("tsibbledata")) {
#' library(tsibble)
#' library(dplyr)
#' fit <- tsibbledata::aus_production %>%
#'   model(
#'     prophet = prophet(Beer ~ season("year", 4, type = "multiplicative"))
#'   )
#'
#' tidy(fit) # coef(fit) or coefficients(fit) can also be used
#' }
#' }
#'
#' @export
tidy.fbl_prophet <- function(x, ...){
  growth_terms <- c("base_growth", "trend_offset")

  seas_terms <- map2(
    x$model$seasonalities, names(x$model$seasonalities),
    function(seas, nm){
      k <- seas[["fourier.order"]]
      paste0(nm, rep(c("_s", "_c"), k), rep(seq_len(k), each = 2))
    }
  )

  hol_terms <- holiday_term_names(x$model)

  xreg_terms <- names(x$model$extra_regressors)

  # Posterior means (a single draw when fitted by optimisation)
  estimate <- c(mean(x$model$params$k), mean(x$model$params$m), colMeans(x$model$params$beta))

  # Report regressor coefficients on the scale of the original data
  if(length(xreg_terms) > 0){
    regr <- x$model$extra_regressors
    idx <- length(estimate) - length(xreg_terms) + seq_along(xreg_terms)
    modes <- map_chr(regr, function(r) r$mode)
    stds <- map_dbl(regr, function(r) r$std)
    scale <- ifelse(modes == "additive", x$model$y.scale, 1)
    estimate[idx] <- estimate[idx] * scale / stds
  }

  tibble(
    term = unlist(c(growth_terms, seas_terms, hol_terms, xreg_terms), use.names = FALSE),
    estimate = estimate
  )
}

#' @export
model_sum.fbl_prophet <- function(x){
  "prophet"
}

#' @export
format.fbl_prophet <- function(x, ...){
  "Prophet Model"
}
