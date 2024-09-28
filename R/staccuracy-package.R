#' Standardized Accuracy and Other Model Performance Metrics
#'
#' This package presents functions for calculating standardized accuracy (staccuracy) based on provided performance metrics. It also provides functions from some classic performance metrics that are highly recommended (such as MAE, RMSE, and AUCROC) as well as their windsorized versions when applicable.
#'
#' @author Chitu Okoli \email{Chitu.Okoli@skema.edu}
#' @docType package
#'
#' @keywords internal
#' @aliases staccuracy-package NULL
#'
#'
#' @import dplyr
#' @importFrom cli cli_abort
#' @importFrom cli cli_alert_danger
#' @importFrom cli cli_alert_info
#' @importFrom cli cli_warn
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr map2_dbl
#' @importFrom purrr map_chr
#' @importFrom purrr map_dbl
#' @importFrom purrr set_names
#' @importFrom purrr walk
#' @importFrom rlang .data
#' @importFrom rlang is_scalar_logical
#' @importFrom stringr str_glue
#'
'_PACKAGE'

# How to document the package: https://roxygen2.r-lib.org/articles/rd-other.html#packages



