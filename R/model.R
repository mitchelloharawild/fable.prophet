#' @export
train_prophet <- function(.data, formula, specials, holidays, quietly = FALSE){
  if(!reticulate::py_module_available("fbprophet")){
    stop("prophet has not yet been installed. Run `install_prophet()` to get started.")
  }

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by Prophet")
  }

  # Prepare data for modelling
  cn <- colnames(.data)
  colnames(.data) <- c("ds", "y")

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

  # Train model
  mdl$fit(.data)
  fits <- mdl$predict(.data)

  # Return model
  colnames(.data) <- cn
  structure(
    list(
      model = mdl,
      est = .data %>% mutate(.fitted = fits$yhat, .resid = !!sym(cn[2]) - fits$yhat),
      components = .data %>% transmute(trend = !!!(fits[c("trend", names(mdl$seasonalities))])),
      formula = formula),
    class = "prophet")
}

specials_prophet <- new_specials_env(
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
    eval_tidy(model.matrix(model_formula), data = .data)
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

#' @export
prophet <- prophet_model$new

#' @export
forecast.prophet <- function(object, new_data, times = 1000, ...){
  # Prepare data and model
  mdl <- object$model
  new_data <- rename(new_data, ds = !!index(new_data))
  new_data <- mdl$setup_dataframe(new_data)
  new_data$trend <- mdl$predict_trend(new_data)

  # Compute predictions without intervals
  mdl$uncertainty_samples <- 0
  pred <- mdl$predict(new_data)

  # Simulate future paths
  mdl$uncertainty_samples <- times
  sim <- mdl$sample_posterior_predictive(new_data)$yhat
  sim <- split(sim, row(sim))

  # Return forecasts
  fablelite::construct_fc(pred$yhat, purrr::map_dbl(sim, sd), dist_sim(sim))
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
