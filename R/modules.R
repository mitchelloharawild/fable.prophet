# global reference to prophet (will be initialized in .onLoad)
prophet <- NULL

.onLoad <- function(libname, pkgname) {
  prophet <<- reticulate::import("fbprophet", delay_load = TRUE)
}
