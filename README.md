
<!-- README.md is generated from README.Rmd. Please edit that file -->
safepredict
===========

[![Travis build status](https://travis-ci.org/alexpghayes/safepredict.svg?branch=master)](https://travis-ci.org/alexpghayes/safepredict) [![Coverage status](https://codecov.io/gh/alexpghayes/safepredict/branch/master/graph/badge.svg)](https://codecov.io/github/alexpghayes/safepredict?branch=master) [![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

The goal of safepredict is to provide consistent predictions via the `safe_predict()` generic. `safe_predict()`:

-   always returns a tibble.
-   never drops rows with missing data.
-   follows a consistent naming convention.

Installation
------------

`safepredict` is currently in the beginning stages of development and is available only on Github. You can install it with:

``` r
# install.packages("devtools")
devtools::install_github("alexpghayes/safepredict")
```

Examples
--------

Suppose you fit a logistic regression using `glm`:

``` r
library(tibble)

data <- tibble(
  y = as.factor(rep(c("A", "B"), each = 50)),
  x = c(rnorm(50, 1), rnorm(50, 3))
)

fit <- glm(y ~ x, data, family = binomial)
```

You can predict class probabilities:

``` r
library(safepredict)

test <- tibble(x = rnorm(10, 2))

safe_predict(fit, new_data = test, type = "prob")
#> # A tibble: 10 x 2
#>   .pred_A .pred_B
#>     <dbl>   <dbl>
#> 1  0.333    0.667
#> 2  0.0410   0.959
#> 3  0.619    0.381
#> 4  0.467    0.533
#> 5  0.132    0.868
#> # ... with 5 more rows
```

or can jump straight to hard class decisions

``` r
safe_predict(fit, new_data = test, type = "class")
#> # A tibble: 10 x 1
#>   .pred_class
#>   <fct>      
#> 1 B          
#> 2 B          
#> 3 A          
#> 4 B          
#> 5 B          
#> # ... with 5 more rows
```

We can also get predictions on the link scale:

``` r
safe_predict(fit, new_data = test, type = "link")
#> # A tibble: 10 x 1
#>    .pred
#>    <dbl>
#> 1  0.696
#> 2  3.15 
#> 3 -0.485
#> 4  0.132
#> 5  1.88 
#> # ... with 5 more rows
```

or we can get confidence intervals on the response scale

``` r
safe_predict(fit, new_data = test, type = "conf_int")
#> # A tibble: 10 x 3
#>   .pred .pred_lower .pred_upper
#>   <dbl>       <dbl>       <dbl>
#> 1 0.667       0.663       0.672
#> 2 0.959       0.957       0.961
#> 3 0.381       0.376       0.386
#> 4 0.533       0.528       0.538
#> 5 0.868       0.864       0.871
#> # ... with 5 more rows
```

Conventions
-----------

`safepredict` is based on a conventions document that is currently in progress and private. Once conventions are finalized, they'll appear here.
