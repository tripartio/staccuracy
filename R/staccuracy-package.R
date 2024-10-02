#' Standardized Accuracy and Other Model Performance Metrics
#'
#'Standardized accuracy (staccuracy) is a framework for expressing accuracy scores such that 50% represents a reference level of performance and 100% is perfect prediction. The 'staccuracy' package provides tools for creating staccuracy functions as well as some recommended staccuracy measures. It also provides functions for some classic performance metrics such as mean absolute error (MAE), root mean squared error (RMSE), and area under the receiver operating characteristic curve (AUCROC), as well as their winsorized versions when applicable.
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



