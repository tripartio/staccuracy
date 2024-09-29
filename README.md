
<!-- README.md is generated from README.Rmd. Please edit that file -->

# staccuracy

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/staccuracy)](https://CRAN.R-project.org/package=staccuracy)
[![R-CMD-check](https://github.com/tripartio/staccuracy/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tripartio/staccuracy/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

This package presents functions for calculating standardized accuracy
(staccuracy) based on provided performance metrics. It also provides
functions from some classic performance metrics that are highly
recommended (such as MAE, RMSE, and AUCROC) as well as their winsorized
versions when applicable.

## Installation

You can install the development version of staccuracy like so:

``` r
# install.packages("devtools")
devtools::install_github("tripartio/staccuracy")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(staccuracy)
#> 
#> Attaching package: 'staccuracy'
#> The following object is masked from 'package:stats':
#> 
#>     mad

# Here's some data
actual_1 <- c(2.3, 4.5, 1.8, 7.6, 3.2)

# Here are some predictions of that data
predicted_1 <- c(2.5, 4.2, 1.9, 7.4, 3.0)

# MAE measures the average error in the predictions
mae(actual_1, predicted_1)
#> [1] 0.2

# But how good is that? 
# MAD gives the natural variation in the actual data; this is a point of comparison.
mad(actual_1)
#> [1] 1.736

# So, our predictions are better (lower) than the MAD, but how good, really?
# Create a standardized accuracy function to give us an easily interpretable metric:
my_mae_vs_mad_sa <- standardized_accuracy(mae, mad)

# Now use it
my_mae_vs_mad_sa(actual_1, predicted_1)
#> [1] 0.9423963

# That's 94.2% standardized accuracy compared to the MAD. Pretty good!

# This and other convenient standardized accuracy scores are already built in
sa_mae_mad(actual_1, predicted_1)  # staccuracy of MAE on MAD
#> [1] 0.9423963
sa_rmse_sd(actual_1, predicted_1)  # staccuracy of RMSE on SD
#> [1] 0.95477
```
