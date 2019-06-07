
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.prophet

[![Travis build
status](https://travis-ci.org/mitchelloharawild/fable.prophet.svg?branch=master)](https://travis-ci.org/mitchelloharawild/fable.prophet)
[![Codecov test
coverage](https://codecov.io/gh/mitchelloharawild/fable.prophet/branch/master/graph/badge.svg)](https://codecov.io/gh/mitchelloharawild/fable.prophet?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

This package provides a tidy R interface to the Prophet forecasting
framework using [fable](https://github.com/tidyverts/fable). This
package makes use of the [prophet](https://github.com/facebook/prophet)
package for R.

## Installation

You can install the development version of fable.prophet from
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

<img src="man/figures/README-plot-1.png" width="100%" />

Each series generally exhibits an increasing trend with an annual
seasonal pattern that varies proportionally to the level of the series.
At a monthly level, any holiday effects can be modelled using a seasonal
term. A piecewise linear trend is included by default, and so it is not
included in the model specification below.

``` r
library(fable.prophet)
#> Loading required package: Rcpp
#> Loading required package: fablelite
fit <- cafe %>% 
  model(
    prophet = prophet(Turnover ~ season("year", 4, type = "multiplicative"))
  )
```

``` r
fit
#> # A mable: 8 x 3
#> # Key:     State, Industry [8]
#>   State                      Industry                              prophet 
#>   <chr>                      <chr>                                 <model> 
#> 1 Australian Capital Territ… Cafes, restaurants and catering serv… <prophe…
#> 2 New South Wales            Cafes, restaurants and catering serv… <prophe…
#> 3 Northern Territory         Cafes, restaurants and catering serv… <prophe…
#> 4 Queensland                 Cafes, restaurants and catering serv… <prophe…
#> 5 South Australia            Cafes, restaurants and catering serv… <prophe…
#> 6 Tasmania                   Cafes, restaurants and catering serv… <prophe…
#> 7 Victoria                   Cafes, restaurants and catering serv… <prophe…
#> 8 Western Australia          Cafes, restaurants and catering serv… <prophe…
```

The above output confirms that this Prophet model has been fitted to
each of the time series. Components from this model can be extracted:

``` r
components(fit)
#> # A dable:               3,432 x 10 [1M]
#> # Key:                   State, Industry, .model [8]
#> # Prophet Decomposition: Turnover = trend * (1 + multiplicative_terms) +
#> #   additive_terms
#>    State Industry .model      Month Turnover additive_terms
#>    <chr> <chr>    <chr>       <mth>    <dbl>          <dbl>
#>  1 Aust… Cafes, … proph…   1982 Apr      4.4              0
#>  2 Aust… Cafes, … proph…   1982 May      3.4              0
#>  3 Aust… Cafes, … proph…   1982 Jun      3.6              0
#>  4 Aust… Cafes, … proph…   1982 Jul      4                0
#>  5 Aust… Cafes, … proph…   1982 Aug      3.6              0
#>  6 Aust… Cafes, … proph…   1982 Sep      4.2              0
#>  7 Aust… Cafes, … proph…   1982 Oct      4.8              0
#>  8 Aust… Cafes, … proph…   1982 Nov      5.4              0
#>  9 Aust… Cafes, … proph…   1982 Dec      6.9              0
#> 10 Aust… Cafes, … proph…   1983 Jan      3.8              0
#> # … with 3,422 more rows, and 4 more variables:
#> #   multiplicative_terms <dbl>, trend <dbl>, year <dbl>, .resid <dbl>
```

<img src="man/figures/README-components-plot-1.png" width="100%" /><img src="man/figures/README-components-plot-2.png" width="100%" />

Note that the annual seasonal pattern does not change very quickly,
although it does differ slightly between years. We can also produce
forecasts for each of these series over the next two years.

``` r
fc <- fit %>% 
  forecast(h = 24)
```

    #> # A fable: 192 x 6 [1M]
    #> # Key:     State, Industry, .model [8]
    #>    State         Industry           .model     Month Turnover .distribution
    #>    <chr>         <chr>              <chr>      <mth>    <dbl> <dist>       
    #>  1 Australian C… Cafes, restaurant… proph…  2018 Oct     45.1 ?            
    #>  2 Australian C… Cafes, restaurant… proph…  2018 Nov     47.6 ?            
    #>  3 Australian C… Cafes, restaurant… proph…  2018 Dec     46.6 ?            
    #>  4 Australian C… Cafes, restaurant… proph…  2019 Jan     40.5 ?            
    #>  5 Australian C… Cafes, restaurant… proph…  2019 Feb     42.9 ?            
    #>  6 Australian C… Cafes, restaurant… proph…  2019 Mar     48.0 ?            
    #>  7 Australian C… Cafes, restaurant… proph…  2019 Apr     46.5 ?            
    #>  8 Australian C… Cafes, restaurant… proph…  2019 May     45.2 ?            
    #>  9 Australian C… Cafes, restaurant… proph…  2019 Jun     46.5 ?            
    #> 10 Australian C… Cafes, restaurant… proph…  2019 Jul     45.6 ?            
    #> # … with 182 more rows

<img src="man/figures/README-fable-1.png" width="100%" />
