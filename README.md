
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ussie

<!-- badges: start -->

[![R-CMD-check](https://github.com/KatherineCox/ussie/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/KatherineCox/ussie/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of ussie is to help you to work with European football league
data supplied by the **`engsoccerdata`** package (Curley 2016).

## Installation

You can install the development version of ussie from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("KatherineCox/ussie")
```

## Example

This is a basic example which shows you how to load data for Italy:

``` r
library(ussie)
uss_make_matches(engsoccerdata::italy, "Italy")
#> # A tibble: 25,404 × 8
#>    country tier  season date       home            visitor       goals…¹ goals…²
#>    <chr>   <fct>  <int> <date>     <chr>           <chr>           <int>   <int>
#>  1 Italy   1       1934 1934-09-30 Lazio Roma      US Livorno          6       1
#>  2 Italy   1       1934 1934-09-30 Torino FC       Unione Tries…       3       1
#>  3 Italy   1       1934 1934-09-30 Sampierdarenese Bologna FC          2       1
#>  4 Italy   1       1934 1934-09-30 SSC Napoli      US Alessandr…       0       1
#>  5 Italy   1       1934 1934-09-30 ACF Fiorentina  AS Roma             4       1
#>  6 Italy   1       1934 1934-09-30 Brescia Calcio  Juventus            0       2
#>  7 Italy   1       1934 1934-09-30 Inter           US Palermo          3       0
#>  8 Italy   1       1934 1934-09-30 Pro Vercelli    AC Milan            1       2
#>  9 Italy   1       1934 1934-10-07 AC Milan        ACF Fiorenti…       1       1
#> 10 Italy   1       1934 1934-10-07 US Livorno      Inter               1       1
#> # … with 25,394 more rows, and abbreviated variable names ¹​goals_home,
#> #   ²​goals_visitor
#> # ℹ Use `print(n = ...)` to see more rows
```
