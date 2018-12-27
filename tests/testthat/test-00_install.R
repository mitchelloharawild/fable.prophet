context("Python package installation test")

test_that("fbprophet installation", {
  skip_if(reticulate::py_module_available("fbprophet"))
  expect_error(model(tsibble::as_tsibble(USAccDeaths), prophet(value)), "install_prophet")
  install_prophet()
})
