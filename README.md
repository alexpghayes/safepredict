
<!-- README.md is generated from README.Rmd. Please edit that file -->

# safepredict

[![Travis build
status](https://travis-ci.org/alexpghayes/safepredict.svg?branch=master)](https://travis-ci.org/alexpghayes/safepredict)
[![Coverage
status](https://codecov.io/gh/alexpghayes/safepredict/branch/master/graph/badge.svg)](https://codecov.io/github/alexpghayes/safepredict?branch=master)
[![lifecycle](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)

`safepredict` has two goals: to provide a consistent interface to
prediction via the `safe_predict()` generic, and to accurately quantify
prediction uncertainty.

`safe_predict()`:

  - always returns a tibble.
  - never drops rows with missing data.
  - always uses the same arguments in the same order.

`safepredict` follows the [tidymodels prediction
specification](https://tidymodels.github.io/model-implementation-principles/model-predictions.html).

## Installation

`safepredict` is currently in the beginning stages of development and is
available only on Github. You can install it with:

``` r
# install.packages("devtools")
devtools::install_github("alexpghayes/safepredict")
```

## Arguments

The three main arguments to `safe_predict()` are always the same:

  - `object`: A model object you would like to get predictions from.

  - `new_data`: **Required**. Data in the same format as required for
    the `predict()` method.

  - `type`: What kind of predictions you would like. Options are:
    
    | type       | application                                 |
    | ---------- | ------------------------------------------- |
    | `response` | numeric predictions                         |
    | `class`    | hard class predictions                      |
    | `prob`     | class probabilities, survivor probabilities |
    | `link`     | `glm` linear predictor                      |
    | `conf_int` | confidence intervals                        |
    | `pred_int` | prediction intervals                        |
    

## Examples

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
#> 1 0.667       0.795       0.510
#> 2 0.959       0.989       0.862
#> 3 0.381       0.545       0.240
#> 4 0.533       0.680       0.380
#> 5 0.868       0.943       0.724
#> # ... with 5 more rows
```

## Related work

  - [`parsnip`](https://tidymodels.github.io/parsnip/) provides a
    consistent interface to supervised models. Eventually `safepredict`
    will act as the prediction backend for `parsnip`.

  - [`broom`](https://broom.tidyverse.org/) summarizes key information
    about models in tidy tibbles.

  - The many ongoing [`tidymodels`](https://github.com/tidymodels)
    projects.

  - The [`prediction`](https://github.com/leeper/prediction) package by
    Thomas Leeper. `prediction` supports a much wider variety of models
    than `safepredict` at the moment, but makes fewer consistency
    guarantees.
