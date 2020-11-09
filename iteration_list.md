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
    ##  [1] 4.242096 2.493184 1.048970 1.376258 3.863464 3.363152 2.250889 2.634872
    ##  [9] 1.743809 2.873582 2.457829 4.092071 3.282886 1.068703 2.377895 3.466934
    ## [17] 3.484497 1.699358 1.998986 4.268412
    ## 
    ## $b
    ##  [1]  2.22446032  1.51833063 -4.26550972 -0.08127051 -3.08048861 -2.43066701
    ##  [7]  1.72598045  3.36269415  6.25671225 -2.28400479 -4.96647120  5.34988845
    ## [13] -4.76262091  6.66078692 -5.21405810  0.64788018 -1.85005042 -5.09272637
    ## [19]  5.34854059 -3.29254475
    ## 
    ## $c
    ##  [1]  9.797021  9.966441 10.096776  9.835518  9.790321  9.952704  9.904674
    ##  [8]  9.919274  9.693935 10.295151 10.035649 10.072663 10.094139 10.340278
    ## [15] 10.196943 10.156952 10.199081 10.159878 10.227517  9.996675
    ## 
    ## $d
    ##  [1] -4.045083 -2.009748 -4.247622 -3.274103 -2.560241 -1.950689 -2.756458
    ##  [8] -3.455275 -1.402761 -2.561116 -4.725430 -1.947141 -1.498838 -2.873164
    ## [15] -3.130898 -2.684540 -3.749212 -3.236325 -1.402163 -3.370605

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
    ## 1  2.70  1.03

``` r
mean_and_sd(list_norms[[2]])
```

    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.211  4.07

``` r
mean_and_sd(list_norms[[3]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.179

``` r
mean_and_sd(list_norms[[4]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84 0.954

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
    ## 1  2.70  1.03
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.211  4.07
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.179
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84 0.954

``` r
# save the name of the objects in the input list
```

What if we want a different function?

``` r
output = map(list_norms, IQR)
```

``` r
output = map_dbl(list_norms, median, .id = "input")
# instead of giving a list, this output several numbers
```

``` r
output = map_df(list_norms, mean_and_sd, .id = "input")
# put the name of the input list as a separate column
```

## List columns

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"),
    samp = list_norms
  )
```

``` r
listcol_df %>% pull(name)
```

    ## [1] "a" "b" "c" "d"

``` r
listcol_df %>% pull(samp)
```

    ## $a
    ##  [1] 4.242096 2.493184 1.048970 1.376258 3.863464 3.363152 2.250889 2.634872
    ##  [9] 1.743809 2.873582 2.457829 4.092071 3.282886 1.068703 2.377895 3.466934
    ## [17] 3.484497 1.699358 1.998986 4.268412
    ## 
    ## $b
    ##  [1]  2.22446032  1.51833063 -4.26550972 -0.08127051 -3.08048861 -2.43066701
    ##  [7]  1.72598045  3.36269415  6.25671225 -2.28400479 -4.96647120  5.34988845
    ## [13] -4.76262091  6.66078692 -5.21405810  0.64788018 -1.85005042 -5.09272637
    ## [19]  5.34854059 -3.29254475
    ## 
    ## $c
    ##  [1]  9.797021  9.966441 10.096776  9.835518  9.790321  9.952704  9.904674
    ##  [8]  9.919274  9.693935 10.295151 10.035649 10.072663 10.094139 10.340278
    ## [15] 10.196943 10.156952 10.199081 10.159878 10.227517  9.996675
    ## 
    ## $d
    ##  [1] -4.045083 -2.009748 -4.247622 -3.274103 -2.560241 -1.950689 -2.756458
    ##  [8] -3.455275 -1.402761 -2.561116 -4.725430 -1.947141 -1.498838 -2.873164
    ## [15] -3.130898 -2.684540 -3.749212 -3.236325 -1.402163 -3.370605

``` r
listcol_df %>% 
  filter(name == "a")
```

    ## # A tibble: 1 x 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>

Let’s try operations.

``` r
mean_and_sd(listcol_df$samp[[1]])
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.03

Can I just map?

``` r
map(listcol_df$samp, mean_and_sd)
```

    ## $a
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.70  1.03
    ## 
    ## $b
    ## # A tibble: 1 x 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.211  4.07
    ## 
    ## $c
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  10.0 0.179
    ## 
    ## $d
    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -2.84 0.954

So can i add a list column?

``` r
listcol_df %>% 
  mutate(
    summary = map(samp, mean_and_sd),
    medians = map_dbl(samp, median))
```

    ## # A tibble: 4 x 4
    ##   name  samp         summary          medians
    ##   <chr> <named list> <named list>       <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>   2.56 
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  -0.966
    ## 3 c     <dbl [20]>   <tibble [1 × 2]>  10.1  
    ## 4 d     <dbl [20]>   <tibble [1 × 2]>  -2.81
