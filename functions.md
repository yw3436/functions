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

    ##  [1] -2.25396120 -1.05764673  0.08683945 -0.02992390  0.90001225  0.01559483
    ##  [7] -0.31922131 -0.35942997 -0.62101216  0.01512216 -0.86303423 -1.27880849
    ## [13]  0.98863939 -0.46632642  0.84785265  0.83466017 -1.26970779 -0.40464985
    ## [19]  1.86249663 -0.59211887  0.39992515 -0.32109565  1.06918433  0.95743661
    ## [25]  1.85917296

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

    ##  [1] -2.25396120 -1.05764673  0.08683945 -0.02992390  0.90001225  0.01559483
    ##  [7] -0.31922131 -0.35942997 -0.62101216  0.01512216 -0.86303423 -1.27880849
    ## [13]  0.98863939 -0.46632642  0.84785265  0.83466017 -1.26970779 -0.40464985
    ## [19]  1.86249663 -0.59211887  0.39992515 -0.32109565  1.06918433  0.95743661
    ## [25]  1.85917296

``` r
# tell z_scores that we will use x_vec for calculation
```

Try my function on some other things, these should be errors

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

## multiple outputs

``` r
mean_and_sd = function(x) {
  if(!is.numeric(x)){
    stop("Input must be numeric")
  }
  if(length(x) < 3) {
    stop("Input must have at least 3 numbers")
  }
  mean_x = mean(x)
  sd_x = sd(x)
  tibble(
    mean = mean_x,
    sd_x = sd_x
  )
}
## for the tibble(), can also use list
```

Check that the function works

``` r
x_vec = rnorm(1000)
mean_and_sd(x_vec)
```

    ## # A tibble: 1 x 2
    ##     mean  sd_x
    ##    <dbl> <dbl>
    ## 1 0.0556  1.02

## multiple inputs

I’d like to do this with a function.

``` r
sim_data = 
  tibble(
    x = rnorm(100, mean = 4, sd = 3)
  )

sim_data %>% 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.00  2.96

``` r
sim_mean_sd = function(samp_size, mu = 3, sigma = 3) {
  # here, we set default value that mu = 3 and sigma = 3 if the user didn't input anything.
  sim_data = 
  tibble(
    x = rnorm(samp_size, mean = mu, sd = sigma)
  )

  sim_data %>% 
    summarize(
      mean = mean(x),
     sd = sd(x)
   )
}

sim_mean_sd(samp_size = 100, mu = 6, sigma = 3) # if we don't specify what 100 is, the r will by default think this is the first thing we input, that is, the sample size
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.72  3.16

``` r
sim_mean_sd(samp_size = 100, sigma = 3, mu = 6)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.08  2.79

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.38  3.51
