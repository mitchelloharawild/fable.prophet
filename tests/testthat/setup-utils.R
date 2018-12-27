skip_if_no_fbprophet <- function() {
  if (!reticulate::py_module_available("fbprophet"))
    skip("Prophet not available for testing")
}
