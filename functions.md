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

    ##  [1]  0.2350117 -0.1395934 -0.6837305 -0.4599400 -0.6306611 -1.0976819
    ##  [7] -0.3145792 -1.2964731  0.7968267  0.1081130  2.2944365  0.3984009
    ## [13] -2.2549599  0.5360374  1.9075366  0.5705936  0.7128807 -0.3777154
    ## [19]  0.3137426  0.8615680 -1.0817726 -0.4415296 -0.9434430  0.8278972
    ## [25]  0.1590348

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

    ##  [1]  0.2350117 -0.1395934 -0.6837305 -0.4599400 -0.6306611 -1.0976819
    ##  [7] -0.3145792 -1.2964731  0.7968267  0.1081130  2.2944365  0.3984009
    ## [13] -2.2549599  0.5360374  1.9075366  0.5705936  0.7128807 -0.3777154
    ## [19]  0.3137426  0.8615680 -1.0817726 -0.4415296 -0.9434430  0.8278972
    ## [25]  0.1590348

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
    ## 1 0.0515 0.974

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
    ## 1  4.61  3.23

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
    ## 1  6.11  3.32

``` r
sim_mean_sd(samp_size = 100, sigma = 3, mu = 6)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.06  2.82

``` r
sim_mean_sd(samp_size = 100)
```

    ## # A tibble: 1 x 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.68  3.08

## let’s review Napoleon Dynamite

``` r
url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

dynamite_html = read_html(url)

review_titles = 
  dynamite_html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  dynamite_html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  dynamite_html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)
```

What about the next page of review? let’s turn the code into a function

``` r
read_page_reviews = function(url) {
  html = read_html(url)

review_titles = 
  html %>%
  html_nodes(".a-text-bold span") %>%
  html_text()

review_stars = 
  html %>%
  html_nodes("#cm_cr-review_list .review-rating") %>%
  html_text() %>%
  str_extract("^\\d") %>%
  as.numeric()

review_text = 
  html %>%
  html_nodes(".review-text-content span") %>%
  html_text() %>% 
  str_replace_all("\n", "") %>% 
  str_trim()

reviews = tibble(
  title = review_titles,
  stars = review_stars,
  text = review_text
)

reviews
}
```

try the function

``` r
dynamite_url = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=2"

read_page_reviews(dynamite_url)
```

    ## # A tibble: 10 x 3
    ##    title                               stars text                               
    ##    <chr>                               <dbl> <chr>                              
    ##  1 Movie is still silly fun....amazon…     1 We are getting really frustrated w…
    ##  2 Brilliant and awkwardly funny.          5 I've watched this movie repeatedly…
    ##  3 Great purchase price for great mov…     5 Great movie and real good digital …
    ##  4 Movie for memories                      5 I've been looking for this movie t…
    ##  5 Love!                                   5 Love this movie. Great quality     
    ##  6 Hilarious!                              5 Such a funny movie, definitely bro…
    ##  7 napoleon dynamite                       5 cool movie                         
    ##  8 Top 5                                   5 Best MOVIE ever! Funny one liners …
    ##  9 👍                                      5 Exactly as described and came on t…
    ## 10 A top favorite movie !!                 5 Love this movie, needed to add it …

Let’s read a few pages of reviews

``` r
dynamite_url_base = "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber="

dynamite_urls = str_c(dynamite_url_base, 1:5)

dynamite_urls[1]
```

    ## [1] "https://www.amazon.com/product-reviews/B00005JNBQ/ref=cm_cr_arp_d_viewopt_rvwer?ie=UTF8&reviewerType=avp_only_reviews&sortBy=recent&pageNumber=1"

``` r
all_reviews = bind_rows(
read_page_reviews(dynamite_urls[1]),
read_page_reviews(dynamite_urls[2]),
read_page_reviews(dynamite_urls[3]),
read_page_reviews(dynamite_urls[4]),
read_page_reviews(dynamite_urls[5])
)
```

## Mean scoping example

``` r
f = function(x){
  z = x + y
  z
}

x = 1
y = 2

f(x = y)
```

    ## [1] 4

``` r
# Give x the value of y
```
