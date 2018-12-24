# global reference to fbprophet (will be initialized in .onLoad)
fbprophet <- NULL

.onLoad <- function(libname, pkgname) {
  fbprophet <<- reticulate::import("fbprophet", delay_load = TRUE)
}
