
<!-- README.md is generated from README.Rmd. Please edit that file -->

# fable.prophet

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
#> The following object is masked from 'package:stats':
#> 
#>     filter
library(ggplot2)
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
#> 
#> Attaching package: 'fablelite'
#> The following object is masked from 'package:stats':
#> 
#>     simulate
fit <- cafe %>% 
  model(
    prophet = prophet(Turnover ~ season("year", 4))
  )
```

``` r
fit
#> # A mable: 8 models
#> # Key:     State, Industry [8]
#>   State                       Industry                              prophet
#>   <chr>                       <chr>                                 <model>
#> 1 Australian Capital Territo… Cafes, restaurants and catering serv… prophet
#> 2 New South Wales             Cafes, restaurants and catering serv… prophet
#> 3 Northern Territory          Cafes, restaurants and catering serv… prophet
#> 4 Queensland                  Cafes, restaurants and catering serv… prophet
#> 5 South Australia             Cafes, restaurants and catering serv… prophet
#> 6 Tasmania                    Cafes, restaurants and catering serv… prophet
#> 7 Victoria                    Cafes, restaurants and catering serv… prophet
#> 8 Western Australia           Cafes, restaurants and catering serv… prophet
```

The above output confirms that this Prophet model has been fitted to
each of the time series. We can then produce forecasts for each of these
series over the next two years.

``` r
fc <- fit %>% 
  forecast(h = 24)
```

    #> # A fable: 192 x 6 [1M]
    #> # Key:     State, Industry, .model [8]
    #>    State         Industry           .model     Month Turnover .distribution
    #>    <chr>         <chr>              <chr>      <mth>    <dbl> <dist>       
    #>  1 Australian C… Cafes, restaurant… proph…  2018 Oct     45.2 sim(dbl[1000…
    #>  2 Australian C… Cafes, restaurant… proph…  2018 Nov     46.4 sim(dbl[1000…
    #>  3 Australian C… Cafes, restaurant… proph…  2018 Dec     46.2 sim(dbl[1000…
    #>  4 Australian C… Cafes, restaurant… proph…  2019 Jan     43.4 sim(dbl[1000…
    #>  5 Australian C… Cafes, restaurant… proph…  2019 Feb     44.1 sim(dbl[1000…
    #>  6 Australian C… Cafes, restaurant… proph…  2019 Mar     46.4 sim(dbl[1000…
    #>  7 Australian C… Cafes, restaurant… proph…  2019 Apr     46.1 sim(dbl[1000…
    #>  8 Australian C… Cafes, restaurant… proph…  2019 May     45.7 sim(dbl[1000…
    #>  9 Australian C… Cafes, restaurant… proph…  2019 Jun     46.3 sim(dbl[1000…
    #> 10 Australian C… Cafes, restaurant… proph…  2019 Jul     46.0 sim(dbl[1000…
    #> # ... with 182 more rows

<img src="man/figures/README-fable-1.png" width="100%" />
