Iteration and List Columns
================
Yuqi Wang

``` r
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ ggplot2 3.3.2     ✓ purrr   0.3.4
    ## ✓ tibble  3.0.3     ✓ dplyr   1.0.2
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.3.1     ✓ forcats 0.5.0

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(rvest)
```

    ## Loading required package: xml2

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:purrr':
    ## 
    ##     pluck

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

## Lists

You can put anything in a list

``` r
l = list(
vec_numeric = 5:8,
vec_char = c("My", "name", "is", "Jeff"),
vec_logical = c(TRUE, TRUE, TRUE, FALSE),
mat = matrix(1:8, nrow = 2, ncol = 4),
summary = summary(rnorm(100))
)
```

``` r
l$vec_numeric
```

    ## [1] 5 6 7 8

``` r
l[[1]]
```

    ## [1] 5 6 7 8

``` r
l[["vec_numeric"]]
```

    ## [1] 5 6 7 8

``` r
mean(l[["vec_numeric"]])
```

    ## [1] 6.5

## `for` loop

create a new list.

``` r
list_norms = 
  list(
    a = rnorm(20, 3, 1),
    b = rnorm(20, 0, 5),
    c = rnorm(20, 10, .2),
    d = rnorm(20, -3, 1)
  )

is.list(list_norms)
```

    ## [1] TRUE

``` r
list_norms
```

    ## $a
    ##  [1] 4.421657 3.546975 4.166788 2.041499 2.900366 3.318506 1.576009 4.353628
    ##  [9] 1.831198 3.189983 3.505663 3.467396 2.627489 3.002374 3.132762 2.083090
    ## [17] 5.126227 2.043729 3.819882 3.295444
    ## 
    ## $b
    ##  [1]  3.0539130  1.8244934 -9.4976778 -3.9436082 11.2164608  6.0816823
    ##  [7] -8.0081858  2.3797944 -2.0270786 -7.6711623 -2.2781598 -4.4776003
    ## [13] -0.8349141  5.2021927  4.3811264 -0.9650770 -9.4724206 -5.6677143
    ## [19] -0.9944489  3.8483772
    ## 
    ## $c
    ##  [1]  9.740193 10.096065 10.055024 10.088923  9.945552  9.825793 10.315499
    ##  [8]  9.935464 10.032463  9.999628 10.094645  9.998169 10.165045 10.164164
    ## [15] 10.068997 10.096185  9.803553  9.930628  9.771639  9.849710
    ## 
    ## $d
    ##  [1] -0.9484025 -3.3013293 -2.5918272 -3.0611413 -2.6783832 -2.1449225
    ##  [7] -1.7767692 -3.7590001 -2.6848272 -5.0486049 -4.7390574 -3.5799919
    ## [13] -1.1256054 -3.8786467 -3.1913463 -2.7591762 -2.6708945 -3.9624278
    ## [19] -1.8688426 -3.1580197

Pause and get the old function.

``` r
mean_and_sd = function(x) {
  
  if (!is.numeric(x)) {
    stop("Argument x should be numeric")
  } else if (length(x) == 1) {
    stop("Cannot be computed for length 1 vectors")
  }
  
  mean_x = mean(x)
  sd_x = sd(x)

  tibble(
    mean = mean_x, 
    sd = sd_x
  )
}
```

I can apply the function to each list element.

``` r
mean_and_sd(list_norms[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.17 0.947

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.893  5.66

``` r
mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.149

``` r
mean_and_sd(list_norms[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.95  1.07

Let’s use a for loop:

``` r
output = vector("list", length = 4)

for (i in 1:4) {
  output[[i]] = mean_and_sd(list_norms[[i]])
}
```

## Let’s try map\!

``` r
map(list_norms, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.17 0.947
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.893  5.66
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.149
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.95  1.07

``` r
# save the name of the objects in the input list
```

What if we want a different function?

``` r
output = map(list_norms, IQR)
```
