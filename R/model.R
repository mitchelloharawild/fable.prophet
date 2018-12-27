globalVariables("self")

train_prophet <- function(.data, formula, specials, holidays, quietly = FALSE){
  if(!reticulate::py_module_available("fbprophet")){
    stop("prophet has not yet been installed. Run `install_prophet()` to get started.")
  }

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by Prophet")
  }

  # Prepare data for modelling
  model_data <- .data
  colnames(model_data) <- c("ds", "y")

  # Growth
  growth <- specials$growth[[1]]
  .data$cap <- growth$capacity
  .data$floor <- growth$floor

  # Holidays
  holiday <- specials$holiday[[1]]

  # Build model
  mdl <- fbprophet$Prophet(
    growth = growth$type,
    changepoint_prior_scale = growth$changepoint_prior_scale,
    n_changepoints = growth$n_changepoints,
    changepoints = growth$changepoints,
    changepoint_range = growth$changepoint_range,
    holidays = holiday$holidays,
    holidays_prior_scale = holiday$prior_scale,
    yearly_seasonality = FALSE,
    weekly_seasonality = FALSE,
    daily_seasonality = FALSE,
    uncertainty_samples = 0
  )

  # Seasonality
  for (season in specials$season){
    mdl$add_seasonality(name = season$name, period = season$period,
                        fourier_order = season$order, prior_scale = season$prior_scale,
                        mode = season$type)
  }

  # Exogenous Regressors
  for(regressor in specials$xreg){
    for(nm in colnames(regressor$xreg)){
      model_data[nm] <- regressor$xreg[,nm]
      mdl$add_regressor(name = nm, prior_scale = regressor$prior_scale,
                        standardize = regressor$standardize, mode = regressor$mode)
    }
  }

  # Train model
  mdl$fit(model_data)
  fits <- mdl$predict(model_data)

  # Return model
  structure(
    list(
      model = mdl,
      est = .data %>% mutate(.fitted = fits$yhat, .resid = !!sym(measured_vars(.data)) - fits$yhat),
      components = .data %>% transmute(trend = !!!(fits[c("trend", names(mdl$seasonalities))])),
      definition = self),
    class = "prophet")
}

specials_prophet <- new_specials(
  growth = function(type = c("linear", "logistic"),
                   capacity = NULL, floor = NULL,
                   changepoints = NULL, n_changepoints = 25,
                   changepoint_range = 0.8, changepoint_prior_scale = 0.05){
    capacity <- eval_tidy(enquo(capacity), data = .data)
    floor <- eval_tidy(enquo(floor), data = .data)
    type <- match.arg(type)
    as.list(environment())
  },
  season = function(period, order, prior_scale = 10,
                    type = c("additive", "multiplicative"),
                    name = as.character(period)){
    force(name)
    period <- parse_period(period)
    order <- as.integer(order)
    type <- match.arg(type)
    as.list(environment())
  },
  holiday = function(holidays = NULL, prior_scale = 10L){
    as.list(environment())
  },
  xreg = function(..., prior_scale = NULL, standardize = "auto", type = NULL){
    model_formula <- new_formula(
      lhs = NULL,
      rhs = purrr::reduce(c(0, enexprs(...)), ~ call2("+", .x, .y))
    )
    list(
      xreg = model.matrix(model_formula, .data),
      prior_scale = prior_scale,
      standardize = standardize,
      mode = type
    )
  },
  .required_specials = c("growth", "holiday")
)

prophet_model <- R6::R6Class("prophet",
                             inherit = fablelite::model_definition,
                             public = list(
                               model = "Prophet",
                               train = train_prophet,
                               specials = specials_prophet
                             )
)

#' Prophet framework modelling
#'
#' Prepares a Prophet model for use within the `fable` package.
#'
#' @param ... Arguments used to fit the model
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
#' season(period, order, prior_scale = 10,
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
#'
#' library(tsibble)
#' tsibbledata::ausretail %>%
#'   filter(Industry == "Cafes, restaurants and catering services") %>%
#'   model(
#'     prophet = prophet(Turnover ~ season("year", 4, type = "multiplicative"))
#'   )
#'
#' @export
prophet <- prophet_model$new

#' @export
forecast.prophet <- function(object, new_data, times = 1000, ...){
  mdl <- object$model

  # Prepare data
  object$definition$data <- new_data
  specials <- parse_model_rhs(object$definition)$specials
  new_data <- rename(new_data, ds = !!index(new_data))
  new_data <- mdl$setup_dataframe(new_data)
  new_data$trend <- mdl$predict_trend(new_data)

  # Growth
  growth <- specials$growth[[1]]
  .data$cap <- growth$capacity
  .data$floor <- growth$floor

  # Simulate future paths
  mdl$uncertainty_samples <- times
  sim <- mdl$sample_posterior_predictive(new_data)$yhat
  sim <- split(sim, row(sim))

  # Exogenous Regressors (for some reason, this must happen after simulation)
  for(regressor in specials$xreg){
    for(nm in colnames(regressor$xreg)){
      new_data[nm] <- regressor$xreg[,nm]
    }
  }

  # Compute predictions without intervals
  mdl$uncertainty_samples <- 0
  pred <- mdl$predict(new_data)

  # Return forecasts
  fablelite::construct_fc(pred$yhat, purrr::map_dbl(sim, stats::sd), dist_sim(sim))
}

#' @export
fitted.prophet <- function(object, ...){
  select(object$est, !!index(object$est), ".fitted")
}

#' @export
residuals.prophet <- function(object, ...){
  select(object$est, !!index(object$est), ".resid")
}

#' @export
augment.prophet <- function(x, ...){
  x$est
}

#' @export
components.prophet <- function(object, ...){
  object$components
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
