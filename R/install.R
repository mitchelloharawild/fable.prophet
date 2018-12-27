#' Install Prophet and its dependencies
#'
#' @inheritParams reticulate::conda_list
#'
#' @param method Installation method. By default, "auto" automatically finds a
#'   method that will work in the local environment. Change the default to force
#'   a specific installation method.
#'
#' @param envname Name of Python environment to install within
#'
#' @param extra_packages Additional Python packages to install along with
#'   Prophet.
#'
#' @export
install_prophet <- function(method = c("auto", "virtualenv", "conda"),
                            conda = "auto",
                            envname = "r-fable-prophet",
                            extra_packages = "pystan") {

  method <- match.arg(method)

  reticulate::py_install(c("fbprophet", extra_packages), envname = envname,
                         method = method, conda = conda)
}
