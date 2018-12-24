#' @export
train_prophet <- function(.data, formula, specials, holidays, quietly = FALSE){
  if(!reticulate::py_module_available("fbprophet")){
    stop("prophet has not yet been installed. Run `install_prophet()` to get started.")
  }

  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by Prophet")
  }

  cn <- colnames(.data)
  colnames(.data) <- c("ds", "y")

  mdl <- fbprophet$Prophet()
  mdl$fit(.data)

  fits <- mdl$predict(.data)

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
  trend = function(type = c("linear", "logistic"), changepoints = NULL, n_changepoints = 25, changepoint_prior_scale = 0.05){
    as.list(environment())
  },
  season = function(period, order, prior_scale = 10, type = c("additive", "multiplicative")){
    as.list(environment())
  },
  xreg = function(..., prior_scale = NULL, standardize = "auto", type = NULL){
    model_formula <- new_formula(
      lhs = NULL,
      rhs = purrr::reduce(c(0, enexprs(...)), ~ call2("+", .x, .y))
    )
    eval_tidy(model.matrix(model_formula), data = .data)
  }
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
