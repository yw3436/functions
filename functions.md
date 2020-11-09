Writing functions
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

## Do something simple

``` r
x_vec = rnorm(25, mean = 5, sd = 3)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1] -0.873444131  0.398788550 -1.503294753 -0.717489232  1.225305840
    ##  [6]  1.009071047  0.267765475 -0.563735242 -1.255273430  0.846495367
    ## [11]  0.156337115 -0.560330063  0.207503441  1.194478835 -0.192747837
    ## [16]  0.400121242 -1.878640658  0.465383878 -0.921760926 -0.288042232
    ## [21]  0.009831292  0.440881680 -0.767799382  0.141228142  2.759365980

I want a function to compute z-scores

``` r
z_scores = function(x) {
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  if(length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  z = (x - mean(x))/sd(x)
  return(z)
}

z_scores(x_vec)
```

    ##  [1] -0.873444131  0.398788550 -1.503294753 -0.717489232  1.225305840
    ##  [6]  1.009071047  0.267765475 -0.563735242 -1.255273430  0.846495367
    ## [11]  0.156337115 -0.560330063  0.207503441  1.194478835 -0.192747837
    ## [16]  0.400121242 -1.878640658  0.465383878 -0.921760926 -0.288042232
    ## [21]  0.009831292  0.440881680 -0.767799382  0.141228142  2.759365980

``` r
# tell z_score that we will use x_vec for calculation
```

Try my function on some other things

``` r
z_scores(3) # the result is NA because sd(3) is meaningless
```

    ## Error in z_scores(3): Input must have at least 3 numbers

``` r
z_scores(mtcars)
```

    ## Error in z_scores(mtcars): Input must be numeric

``` r
z_scores("mtcars")
```

    ## Error in z_scores("mtcars"): Input must be numeric

``` r
z_scores(c(TRUE, TRUE, FALSE, TRUE)) # this works but meaningless
```

    ## Error in z_scores(c(TRUE, TRUE, FALSE, TRUE)): Input must be numeric

so we nned ot update the function by adding if()
