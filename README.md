
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.prophet

[![R-CMD-check](https://github.com/mitchelloharawild/fable.prophet/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/mitchelloharawild/fable.prophet/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/mitchelloharawild/fable.prophet/branch/main/graph/badge.svg)](https://app.codecov.io/gh/mitchelloharawild/fable.prophet?branch=main)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/fable.prophet)](https://cran.r-project.org/package=fable.prophet)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)

This package provides a tidy R interface to the prophet forecasting
procedure using [fable](https://github.com/tidyverts/fable). This
package makes use of the [prophet
package](https://cran.r-project.org/package=prophet) for R.

## Installation

The can install the **stable** version from
[CRAN](https://cran.r-project.org/package=fable.prophet):

``` r
install.packages("fable.prophet")
```

You can install the **development** version from
[Github](https://github.com/mitchelloharawild/fable.prophet) with:

``` r
# install.packages("remotes")
remotes::install_github("mitchelloharawild/fable.prophet")
```

## Example

Suppose we wanted to model Australia’s monthly turnover for cafes,
restaurants and catering services. The data is available from the
Australian Bureau of Statistics catalogue 8501.0, and in the
[tsibbledata](https://github.com/tidyverts/tsibbledata) package.

``` r
library(tsibble)
library(dplyr)
cafe <- tsibbledata::aus_retail %>% 
  filter(Industry == "Cafes, restaurants and catering services")
```

<img src="man/figures/README-plot-1.png" alt="" width="100%" />

Each series generally exhibits an increasing trend with an annual
seasonal pattern that varies proportionally to the level of the series.
At a monthly level, any holiday effects can be modelled using a seasonal
term. A piecewise linear trend is included by default, and so it is not
included in the model specification below.

``` r
library(fable.prophet)
#> Loading required package: fabletools
fit <- cafe %>% 
  model(
    prophet = prophet(Turnover ~ season("year", 4, type = "multiplicative"))
  )
```

``` r
fit
#> # A mable: 8 x 3
#> # Key:     State, Industry [8]
#>   State                        Industry                                  prophet
#>   <chr>                        <chr>                                     <model>
#> 1 Australian Capital Territory Cafes, restaurants and catering servic… <prophet>
#> 2 New South Wales              Cafes, restaurants and catering servic… <prophet>
#> 3 Northern Territory           Cafes, restaurants and catering servic… <prophet>
#> 4 Queensland                   Cafes, restaurants and catering servic… <prophet>
#> 5 South Australia              Cafes, restaurants and catering servic… <prophet>
#> 6 Tasmania                     Cafes, restaurants and catering servic… <prophet>
#> 7 Victoria                     Cafes, restaurants and catering servic… <prophet>
#> 8 Western Australia            Cafes, restaurants and catering servic… <prophet>
```

The above output confirms that this Prophet model has been fitted to
each of the time series. Components from this model can be extracted:

``` r
components(fit)
#> # A dable: 3,456 x 10 [1M]
#> # Key:     State, Industry, .model [8]
#> # :        Turnover = trend * (1 + multiplicative_terms) + additive_terms +
#> #   .resid
#>    State   Industry .model    Month Turnover additive_terms multiplicative_terms
#>    <chr>   <chr>    <chr>     <mth>    <dbl>          <dbl>                <dbl>
#>  1 Austra… Cafes, … proph… 1982 Apr      4.4              0              0.0149 
#>  2 Austra… Cafes, … proph… 1982 May      3.4              0             -0.0132 
#>  3 Austra… Cafes, … proph… 1982 Jun      3.6              0              0.0103 
#>  4 Austra… Cafes, … proph… 1982 Jul      4                0             -0.0132 
#>  5 Austra… Cafes, … proph… 1982 Aug      3.6              0              0.00987
#>  6 Austra… Cafes, … proph… 1982 Sep      4.2              0              0.0285 
#>  7 Austra… Cafes, … proph… 1982 Oct      4.8              0              0.00107
#>  8 Austra… Cafes, … proph… 1982 Nov      5.4              0              0.0479 
#>  9 Austra… Cafes, … proph… 1982 Dec      6.9              0              0.0239 
#> 10 Austra… Cafes, … proph… 1983 Jan      3.8              0             -0.106  
#> # ℹ 3,446 more rows
#> # ℹ 3 more variables: trend <dbl>, year <dbl>, .resid <dbl>
```

<img src="man/figures/README-components-plot-1.png" alt="" width="100%" /><img src="man/figures/README-components-plot-2.png" alt="" width="100%" />

Note that the annual seasonal pattern does not change very quickly,
although it does differ slightly between years. A very differently
seasonal pattern can be seen for the Northern Territory. We can also
produce forecasts for each of these series over the next two years.

``` r
fc <- fit %>% 
  forecast(h = 24)
```

    #> # A fable: 192 x 6 [1M]
    #> # Key:     State, Industry, .model [8]
    #>    State                        Industry      .model    Month     Turnover .mean
    #>    <chr>                        <chr>         <chr>     <mth>       <dist> <dbl>
    #>  1 Australian Capital Territory Cafes, resta… proph… 2019 Jan sample[5000]  40.4
    #>  2 Australian Capital Territory Cafes, resta… proph… 2019 Feb sample[5000]  42.9
    #>  3 Australian Capital Territory Cafes, resta… proph… 2019 Mar sample[5000]  48.0
    #>  4 Australian Capital Territory Cafes, resta… proph… 2019 Apr sample[5000]  46.4
    #>  5 Australian Capital Territory Cafes, resta… proph… 2019 May sample[5000]  45.3
    #>  6 Australian Capital Territory Cafes, resta… proph… 2019 Jun sample[5000]  46.5
    #>  7 Australian Capital Territory Cafes, resta… proph… 2019 Jul sample[5000]  45.6
    #>  8 Australian Capital Territory Cafes, resta… proph… 2019 Aug sample[5000]  46.7
    #>  9 Australian Capital Territory Cafes, resta… proph… 2019 Sep sample[5000]  47.8
    #> 10 Australian Capital Territory Cafes, resta… proph… 2019 Oct sample[5000]  46.6
    #> # ℹ 182 more rows
    #> Warning: `autolayer.fbl_ts()` was deprecated in fabletools 0.6.0.
    #> ℹ Please use `ggtime::autolayer.fbl_ts()` instead.
    #> ℹ Graphics functions have been moved to the {ggtime} package. Please use
    #>   `library(ggtime)` instead.
    #> This warning is displayed once per session.
    #> Call `lifecycle::last_lifecycle_warnings()` to see where this warning was
    #> generated.

<img src="man/figures/README-fable-1.png" alt="" width="100%" />
