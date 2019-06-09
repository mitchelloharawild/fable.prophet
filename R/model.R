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
    yearly.seasonality = FALSE,
    weekly.seasonality = FALSE,
    daily.seasonality = FALSE,
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
  mdl$uncertainty_samples <- 0
  fits <- predict(mdl, model_data)

  # Return model
  structure(
    list(
      model = mdl,
      est = tibble(.fitted = fits$yhat, .resid = model_data[["y"]] - fits$yhat),
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
                    name = as.character(period)){
    force(name)

    # Extract data interval
    interval <- tsibble::interval(self$data)
    interval <- with(interval, lubridate::years(year) +
                       lubridate::period(3*quarter + month, units = "month") + lubridate::weeks(week) +
                       lubridate::days(day) + lubridate::hours(hour) + lubridate::minutes(minute) +
                       lubridate::seconds(second) + lubridate::milliseconds(millisecond) +
                       lubridate::microseconds(microsecond) + lubridate::nanoseconds(nanosecond))

    # Compute prophet interval
    period <- get_frequencies(period, self$data, .auto = "smallest")
    period <- suppressMessages(interval * period/lubridate::days(1))

    if(is.null(order)){
      if(period %in% c(365.25, 7, 1)){
        order <- c(10, 3, 4)[period == c(365.25, 7, 1)]
      }
      else{
        abort(sprintf("Unable to add %s to the model. The fourier order has no default, and must be specified with `order = ?`.", match.call()))
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

#' Prophet framework modelling
#'
#' Prepares a Prophet model for use within the `fable` package.
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
#'   `type`                    \tab The type of seasonality.\cr
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
#' \preformatted{
#' season(period = NULL, order = NULL, prior_scale = 10,
#'        type = c("additive", "multiplicative"), name = as.character(period))
#' }
#'
#' \tabular{ll}{
#'   `period`      \tab The periodic nature of the seasonality. If a number is given, this will be treated as days in the seasonal period. If a character is given, it will be parsed using `lubridate::as.period`, allowing seasonal periods such as "2 years".\cr
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
#'   `holidays`    \tab A `tsibble` containing a set of holiday events. The event name is given in the 'holiday' column, and the event date is given via the index. Additionally, "lower_window" and "upper_window" columns can be used to include days before and after the holiday.\cr
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
#' - [Prophet homepage](https://facebook.github.io/prophet/)
#' - [Prophet R package](https://cran.r-project.org/web/packages/prophet/index.html)
#' - [Prophet Python package](https://pypi.org/project/fbprophet/)
#'
#' @examples
#' library(tsibble)
#' library(dplyr)
#' tsibbledata::aus_retail %>%
#'   filter(Industry == "Cafes, restaurants and catering services") %>%
#'   model(
#'     prophet = prophet(Turnover ~ season("year", 4, type = "multiplicative"))
#'   )
#'
#' @export
prophet <- function(formula, ...){
  prophet_model <- new_model_class("prophet", train_prophet, specials_prophet)
  new_model_definition(prophet_model, !!enquo(formula), ...)
}

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
  sim <- prophet::predictive_samples(mdl, new_data)$yhat
  sim <- split(sim, row(sim))

  # Return forecasts
  fablelite::construct_fc(pred$yhat, unname(map_dbl(sim, sd)), dist_sim(sim))
}

#' @export
fitted.prophet <- function(object, ...){
  object$est[[".fitted"]]
}

#' @export
residuals.prophet <- function(object, ...){
  object$est[[".resid"]]
}

#' @export
components.prophet <- function(object, ...){
  cmp <- object$components
  cmp$.resid <- object$est$.resid
  mv <- measured_vars(cmp)
  as_dable(cmp, resp = !!sym(mv[1]), method = "Prophet",
           aliases = set_names(
             list(expr(!!sym("trend") * (1 + !!sym("multiplicative_terms") + !!sym("additive_terms")))),
             mv[1]
           )
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

#' @export
print.prophet <- function(x, ...){
  cat(format(x))
}
