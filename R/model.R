#' @export
train_prophet <- function(.data){
  if(!reticulate::py_module_available("fbprophet")){
    stop("prophet has not yet been installed. Run `install_prophet()` to get started.")
  }
  mdl <- prophet$Prophet()
  structure(list(model = mdl$fit(.data)), class = "prophet")
}

#' @export
forecast.prophet <- function(object, new_data){
  fc <- object$model$predict(new_data)
  as_tsibble(fc)
}
