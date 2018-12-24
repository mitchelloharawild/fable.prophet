#' @export
install_prophet <- function(method = c("auto", "virtualenv", "conda"),
                            conda = "auto",
                            envname = "r-fable-prophet",
                            extra_packages = "pystan") {

  method <- match.arg(method)
  if (identical(method, "virtualenv") && is_windows()) {
    stop("Installing prophet into a virtualenv is not supported on Windows",
         call. = FALSE)
  }

  reticulate::py_install(c("fbprophet", extra_packages), envname = envname,
                         method = method, conda = conda)
}
