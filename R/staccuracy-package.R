#' Standardized Accuracy and Other Model Performance Metrics
#'
#' This package presents functions for calculating standardized accuracy (staccuracy) based on provided performance metrics. It also provides functions from some classic performance metrics that are highly recommended (such as MAE, RMSE, and AUCROC) as well as their winsorized versions when applicable.
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
#' @importFrom purrr map
#' @importFrom purrr set_names
#' @importFrom rlang .data
#'
'_PACKAGE'

# How to document the package: https://roxygen2.r-lib.org/articles/rd-other.html#packages



