#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables("self")

#' @importFrom stats predict
train_prophet <- function(.data, specials){
  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by Prophet")
  }

  # Prepare data for modelling
  model_data <- as_tibble(.data)[c(expr_text(index(.data)), measured_vars(.data))]
  colnames(model_data) <- c("ds", "y")

  # Growth
  growth <- specials$growth[[1]]
  model_data$cap <- growth$capacity
  model_data$floor <- growth$floor

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
    yearly.seasonality = is.name(self$formula),
    weekly.seasonality = is.name(self$formula),
    daily.seasonality = is.name(self$formula),
    uncertainty_samples = 0
  )

  # Seasonality
  for (season in specials$season){
    mdl <- prophet::add_seasonality(
      mdl, name = season$name, period = season$period,
      fourier.order = season$order, prior.scale = season$prior_scale,
      mode = season$type)
  }

  # Exogenous Regressors
  for(regressor in specials$xreg){
    for(nm in colnames(regressor$xreg)){
      model_data[nm] <- regressor$xreg[,nm]
      mdl <- prophet::add_regressor(
        mdl, name = nm, prior.scale = regressor$prior_scale,
        standardize = regressor$standardize, mode = regressor$mode)
    }
  }

  # Train model
  mdl <- prophet::fit.prophet(mdl, model_data)
  mdl$uncertainty.samples <- 0
  fits <- predict(mdl, model_data)

  # Return model
  structure(
    list(
      model = mdl,
      est = list(.fitted = fits$yhat, .resid = model_data[["y"]] - fits$yhat),
      components = .data %>% mutate(!!!(fits[c("additive_terms", "multiplicative_terms", "trend", names(mdl$seasonalities))]))),
    class = "prophet")
}

specials_prophet <- new_specials(
  growth = function(type = c("linear", "logistic"),
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
                    name = NULL){
    # Extract data interval
    interval <- tsibble::interval(self$data)
    interval <- with(interval, lubridate::years(year) +
                       lubridate::period(3*quarter + month, units = "month") + lubridate::weeks(week) +
                       lubridate::days(day) + lubridate::hours(hour) + lubridate::minutes(minute) +
                       lubridate::seconds(second) + lubridate::milliseconds(millisecond) +
                       lubridate::microseconds(microsecond) + lubridate::nanoseconds(nanosecond))

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
  holiday = function(holidays = NULL, prior_scale = 10L){
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
#' @param ... Not used
#'
#' @section Specials:
#'
#' \subsection{growth}{
#' The `growth` special is used to specify the trend parameters.
#' \preformatted{
#' growth(type = c("linear", "logistic"), capacity = NULL, floor = NULL,
#'        changepoints = NULL, n_changepoints = 25, changepoint_range = 0.8,
#'        changepoint_prior_scale = 0.05)
#' }
#'
#' \tabular{ll}{
#'   `type`                    \tab The type of trend (linear or logistic).\cr
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
#'        type = c("additive", "multiplicative"), name = NULL)
#' }
#'
#' \tabular{ll}{
#'   `period`      \tab The periodic nature of the seasonality. If a number is given, it will specify the number of observations in each seasonal period. If a character is given, it will be parsed using `lubridate::as.period`, allowing seasonal periods such as "2 years".\cr
#'   `order`       \tab The number of terms in the partial Fourier sum. The higher the `order`, the more flexible the seasonality can be.\cr
#'   `prior_scale` \tab Used to control the amount of regularisation applied. Reducing this will dampen the seasonal effect.\cr
#'   `type`        \tab The nature of the seasonality. If "additive", the variability in the seasonal pattern is fixed. If "multiplicative", the seasonal pattern varies proportionally to the level of the series.\cr
#'   `name`        \tab The name of the seasonal term (allowing you to name an annual pattern as 'annual' instead of 'year' or `365.25` for example).\cr
#' }
#' }
#'
#' \subsection{holiday}{
#' The `holiday` special is used to specify a `tsibble` containing holidays for the model.
#' \preformatted{
#' holiday(holidays = NULL, prior_scale = 10L)
#' }
#'
#' \tabular{ll}{
#'   `holidays`    \tab A [`tsibble`](https://tsibble.tidyverts.org/) containing a set of holiday events. The event name is given in the 'holiday' column, and the event date is given via the index. Additionally, "lower_window" and "upper_window" columns can be used to include days before and after the holiday.\cr
#'   `prior_scale` \tab Used to control the amount of regularisation applied. Reducing this will dampen the holiday effect.\cr
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
#' @seealso
#' - [`prophet::prophet()`]
#' - [Prophet homepage](https://facebook.github.io/prophet/)
#' - [Prophet R package](https://CRAN.R-project.org/package=prophet)
#' - [Prophet Python package](https://pypi.org/project/fbprophet/)
#'
#' @examples
#' \dontrun{
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_production %>%
#'   model(
#'     prophet = prophet(Beer ~ season("year", 4, type = "multiplicative"))
#'   )
#' }
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
#' @param ... Additional arguments passed to [`prophet::predict.prophet()`].
#'
#' @seealso [`prophet::predict.prophet()`]
#'
#' @return A list of forecasts.
#'
#' @examples
#'
#' \dontrun{
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_production %>%
#'   model(
#'     prophet = prophet(Beer ~ season("year", 4, type = "multiplicative"))
#'   ) %>%
#'   forecast()
#' }
#'
#' @export
forecast.prophet <- function(object, new_data, specials = NULL, times = 1000, ...){
  mdl <- object$model

  # Prepare data
  new_data <- rename(as.data.frame(new_data), ds = !!index(new_data))

  ## Growth
  growth <- specials$growth[[1]]
  if(!is.null(growth$capacity)){
    new_data$cap <- growth$capacity
  }
  if(!is.null(growth$floor)){
    new_data$floor <- growth$floor
  }

  ## Exogenous Regressors
  for(regressor in specials$xreg){
    for(nm in colnames(regressor$xreg)){
      new_data[nm] <- regressor$xreg[,nm]
    }
  }

  # Compute predictions without intervals
  mdl$uncertainty_samples <- 0
  pred <- predict(mdl, new_data)

  # Simulate future paths
  mdl$uncertainty_samples <- times
  sim <- prophet::predictive_samples(mdl, new_data, ...)$yhat
  sim <- split(sim, row(sim))

  # Return forecasts
  construct_fc(pred$yhat, unname(map_dbl(sim, stats::sd)), dist_sim(sim))
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
fitted.prophet <- function(object, ...){
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
residuals.prophet <- function(object, ...){
  object$est[[".resid"]]
}

#' Extract meaningful components
#'
#' A prophet model consists of terms which are additively or multiplicatively
#' included in the model. Multiplicative terms are scaled proportionally to the
#' estimated trend, while additive terms are not.
#'
#' Extracting a prophet model's components using this function allows you to
#' visualise the components in a similar way to [`prophet::prophet_plot_components()`].
#'
#' @inheritParams fable::components.ETS
#'
#' @return A [`fabletools::dable()`] containing estimated states.
#'
#' @examples
#' \dontrun{
#' library(tsibble)
#' library(dplyr)
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
#'
#' @export
components.prophet <- function(object, ...){
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

#' Extract estimated coefficients from a prophet model
#'
#' @inheritParams fable::tidy.ARIMA
#'
#' @export
tidy.prophet <- function(x, ...){
  growth_terms <- c("base_growth", "trend_offset")

  seas_terms <- map2(
    x$model$seasonalities, names(x$model$seasonalities),
    function(seas, nm){
      k <- seas[["fourier.order"]]
      paste0(nm, rep(c("_s", "_c"), k), rep(seq_len(k), each = 2))
    }
  )

  hol_terms <- map2(
    x$model$holidays$holiday,
    map2(x$model$holidays[["lower_window"]]%||%0, x$model$holidays[["upper_window"]]%||%0, seq),
    function(nm, window){
      window <- ifelse(sign(window) == 1, paste0("_+", window), ifelse(sign(window) == -1, paste0("_", window), ""))
      paste0(nm, window)
    }
  )

  xreg_terms <- names(x$model$extra_regressors)

  tibble(
    term = invoke(c, c(growth_terms, seas_terms, hol_terms, xreg_terms)),
    estimate = c(x$model$params$k, x$model$params$m, x$model$params$beta)
  )
}

#' @export
model_sum.prophet <- function(x){
  "prophet"
}

#' @export
format.prophet <- function(x, ...){
  "Prophet Model"
}
