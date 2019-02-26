
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.prophet

[![Travis build
status](https://travis-ci.org/mitchelloharawild/fable.prophet.svg?branch=master)](https://travis-ci.org/mitchelloharawild/fable.prophet)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

This package provides a tidy R interface to the Prophet forecasting
framework using [fable](https://github.com/tidyverts/fable). This
package makes use of the Python version of
[Prophet](https://github.com/facebook/prophet) via the
[reticulate](https://github.com/rstudio/reticulate) package.

## Installation

You can install the development version of fable.prophet from
[Github](https://github.com/mitchelloharawild/fable.prophet) with:

``` r
devtools::install_github("mitchelloharawild/fable.prophet")
fable.prophet::install_prophet()
```

## Example

Suppose we wanted to model Australia’s monthly turnover for cafes,
restaurants and catering services. The data is available from the
Australian Bureau of Statistics catalogue 8501.0, and in the
[tsibbledata](https://github.com/tidyverts/tsibbledata) package.

``` r
library(tsibble)
#> 
#> Attaching package: 'tsibble'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
cafe <- tsibbledata::ausretail %>% 
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
#> # A tsibble: 3,432 x 6 [1M]
#> # Key:       State, Industry, .model [8]
#>    State            Industry                .model     Month trend     year
#>    <chr>            <chr>                   <chr>      <mth> <dbl>    <dbl>
#>  1 Australian Capi… Cafes, restaurants and… proph…  1982 Apr  4.50  0.0129 
#>  2 Australian Capi… Cafes, restaurants and… proph…  1982 May  4.56 -0.0161 
#>  3 Australian Capi… Cafes, restaurants and… proph…  1982 Jun  4.62  0.00864
#>  4 Australian Capi… Cafes, restaurants and… proph…  1982 Jul  4.68 -0.0155 
#>  5 Australian Capi… Cafes, restaurants and… proph…  1982 Aug  4.74  0.00690
#>  6 Australian Capi… Cafes, restaurants and… proph…  1982 Sep  4.80  0.0277 
#>  7 Australian Capi… Cafes, restaurants and… proph…  1982 Oct  4.86  0.00397
#>  8 Australian Capi… Cafes, restaurants and… proph…  1982 Nov  4.93  0.0559 
#>  9 Australian Capi… Cafes, restaurants and… proph…  1982 Dec  4.99  0.0316 
#> 10 Australian Capi… Cafes, restaurants and… proph…  1983 Jan  5.05 -0.106  
#> # … with 3,422 more rows
```

    #> 
    #> Attaching package: 'lubridate'
    #> The following objects are masked from 'package:tsibble':
    #> 
    #>     interval, new_interval
    #> The following object is masked from 'package:base':
    #> 
    #>     date

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
    #>    State        Industry           .model      Month Turnover .distribution
    #>    <chr>        <chr>              <chr>       <mth>    <dbl> <dist>       
    #>  1 Australian … Cafes, restaurant… prophet  2018 Oct     45.0 sim(=dbl[100…
    #>  2 Australian … Cafes, restaurant… prophet  2018 Nov     47.6 sim(=dbl[100…
    #>  3 Australian … Cafes, restaurant… prophet  2018 Dec     46.6 sim(=dbl[100…
    #>  4 Australian … Cafes, restaurant… prophet  2019 Jan     40.5 sim(=dbl[100…
    #>  5 Australian … Cafes, restaurant… prophet  2019 Feb     42.9 sim(=dbl[100…
    #>  6 Australian … Cafes, restaurant… prophet  2019 Mar     48.0 sim(=dbl[100…
    #>  7 Australian … Cafes, restaurant… prophet  2019 Apr     46.4 sim(=dbl[100…
    #>  8 Australian … Cafes, restaurant… prophet  2019 May     45.2 sim(=dbl[100…
    #>  9 Australian … Cafes, restaurant… prophet  2019 Jun     46.5 sim(=dbl[100…
    #> 10 Australian … Cafes, restaurant… prophet  2019 Jul     45.6 sim(=dbl[100…
    #> # … with 182 more rows

<img src="man/figures/README-fable-1.png" width="100%" />
