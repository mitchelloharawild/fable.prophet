#' @docType package
#' @keywords package
"_PACKAGE"

globalVariables("self")

#' @importFrom stats predict
train_prophet <- function(.data, specials, ...){
  if(length(tsibble::measured_vars(.data)) > 1){
    abort("Only univariate responses are supported by Prophet")
  }

  # Prepare data for modelling
  model_data <- as_tibble(.data)[c(index_var(.data), measured_vars(.data))]
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

  if(!is.null(holiday$country)){
    mdl <- prophet::add_country_holidays(mdl, holiday$country)
  }

  # Seasonality
  for (season in specials$season){
    mdl <- prophet::add_seasonality(
      mdl, name = season$name, period = season$period,
      fourier.order = season$order, prior.scale = season$prior_scale,
      mode = season$type, condition.name = season$condition_name)
    if(!is.null(season$condition_name)){
      model_data[[season$condition_name]] <- season$condition_values
    }
  }

  # Exogenous Regressors
  xreg_names <- xreg_safe_names(specials$xreg)
  i <- 0L
  for(regressor in specials$xreg){
    for(j in seq_len(ncol(regressor$xreg))){
      i <- i + 1L
      nm <- xreg_names[[i]]
      model_data[nm] <- regressor$xreg[,j]
      mdl <- prophet::add_regressor(
        mdl, name = nm, prior.scale = regressor$prior_scale,
        standardize = regressor$standardize, mode = regressor$mode)
    }
  }

  # Train model
  mdl <- prophet::fit.prophet(mdl, model_data, ...)
  mdl$uncertainty.samples <- 0
  fits <- predict(mdl, model_data)

  # Return model
  structure(
    list(
      model = mdl,
      est = list(.fitted = fits$yhat, .resid = model_data[["y"]] - fits$yhat),
      components = .data %>% mutate(!!!(fits[c("additive_terms", "multiplicative_terms", "trend", names(mdl$seasonalities))]))),
    class = "fbl_prophet")
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
#' @inheritParams prophet::fit.prophet
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

  ## Growth
  growth <- specials$growth[[1]]
  if(!is.null(growth$capacity)){
    new_data$cap <- growth$capacity
  }
  if(!is.null(growth$floor)){
    new_data$floor <- growth$floor
  }

  ## Conditional seasonality
  for(season in specials$season){
    if(!is.null(season$condition_name)){
      new_data[[season$condition_name]] <- season$condition_values
    }
  }

  ## Exogenous Regressors
  xreg_names <- xreg_safe_names(specials$xreg)
  i <- 0L
  for(regressor in specials$xreg){
    for(j in seq_len(ncol(regressor$xreg))){
      i <- i + 1L
      new_data[xreg_names[[i]]] <- regressor$xreg[,j]
    }
  }

  # Simulate future paths
  mdl$uncertainty.samples <- times
  sim <- prophet::predictive_samples(mdl, new_data)$yhat
  sim <- split(sim, row(sim))

  # Return forecasts
  distributional::dist_sample(sim)
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
    adjustment = as.numeric(x$model$params$delta)
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

  estimate <- c(x$model$params$k, x$model$params$m, colMeans(x$model$params$beta))

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
