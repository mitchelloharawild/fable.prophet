Examples may have a long elapsed time (especially on win-builder). This is due
because the prophet model (which this package provides a wrapper to) compiles a
stan model the first time a model is ran. The prophet package gets around this 
check issue by wrapping all examples in \dontrun{}, this submission does instead
wraps long-running examples in \donttest{} as requested in a previous submission
attempt.

## Test environments
* local ubuntu 18.04 install, R 3.6.3
* ubuntu 16.04 (on GitHub actions), R 4.0.0, R 3.6.3, R 3.5.3
* macOS (on GitHub actions), R-devel, R 4.0.0
* windows (on GitHub actions), R 3.6.3
* win-builder, R-devel, R-release, R-oldrelease

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release.

## Resubmission

* Removed \dontrun{} from examples
