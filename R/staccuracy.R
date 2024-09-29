#' Standardized accuracy (staccuracy) functions.
#'
#' @export
#' @rdname staccuracy
#'
#' @description
#' Standardized accuracy (staccuracy) represents error or accuracy measures on a scale where 1 or 100% means perfect prediction and 0.5 or 50% is a reference comparison of some specified standard performance. Higher than 0.5 is better than the reference and below 0.5 is worse. 0 might or might not have a special meaning; sometimes negative scores are possible, but these often indicate modelling errors.
#'
#' The core function is `standardized_accuracy()`, which receives as input a generic error function and a reference function against which to compare the error function performance. In addition, the following recommended staccuracy functions are provided:
#' * `sa_mae_mad`: standardized accuracy of the mean absolute error (MAE) based on the mean absolute deviation (MAD)
#' * `sa_rmse_sd`: standardized accuracy of the root mean squared error (RMSE) based on the standard deviation (SD)
#' * `sa_wmae_mad`: standardized accuracy of the winsorized mean absolute error (MAE) based on the mean absolute deviation (MAD)
#' * `sa_wrmse_sd`: standardized accuracy of the winsorized root mean squared error (RMSE) based on the standard deviation (SD)
#'
#' @param error_fun function. The unquoted name of the function that calculates the error (or accuracy) measure. This function must be of the signature `function(actual, pred, na.rm = FALSE)`.
#' @param ref_fun function. The unquoted name of the function that calculates the reference error, accuracy, or deviation measure. This function must be of the signature `ref_fun(actual, na.rm = FALSE)`.
#' @param actual numeric. The true (actual) labels.
#' @param pred numeric. The predicted estimates. Must be the same length as `actual`.
#' @param na.rm logical(1). Whether NA values should be removed (`TRUE`) or not (`FALSE`, default).
#'
#' @details
#' The core function `standardized_accuracy()` receives as input a generic error function and a reference function against which to compare the error function's performance. These input functions must have the following signatures (see the argument specifications for details of the arguments):
#' * `error_fun`: `function(actual, pred, na.rm = na.rm)`; the output must be a scalar numeric (that is, a single number).
#' * `error_fun`: `function(actual, pred, na.rm = na.rm)`; the output must be a scalar numeric (that is, a single number).
#'
#' @returns
#' `standardized_accuracy()` returns a function with signature `function(actual, pred, na.rm = FALSE)` that receives an `actual` and a `pred` vector as inputs and returns the staccuracy of the originally input error function based on the input reference function.
#'
#' The convenience `sa_*()` functions return the staccuracy measures specified above.
#'
#' @examples
#' # Here's some data
#' actual_1 <- c(2.3, 4.5, 1.8, 7.6, 3.2)
#'
#' # Here are some predictions of that data
#' predicted_1 <- c(2.5, 4.2, 1.9, 7.4, 3.0)
#'
#' # MAE measures the average error in the predictions
#' mae(actual_1, predicted_1)
#'
#' # But how good is that?
#' # MAD gives the natural variation in the actual data; this is a point of comparison.
#' mad(actual_1)
#'
#' # So, our predictions are better (lower) than the MAD, but how good, really?
#' # Create a standardized accuracy function to give us an easily interpretable metric:
#' my_mae_vs_mad_sa <- standardized_accuracy(mae, mad)
#'
#' # Now use it
#' my_mae_vs_mad_sa(actual_1, predicted_1)
#'
#' # That's 94.2% standardized accuracy compared to the MAD. Pretty good!
#'
#'
standardized_accuracy <- function(
  error_fun,
  ref_fun
) {
  return(
    function(actual, pred, na.rm = FALSE) {
      e_p <- error_fun(actual, pred, na.rm = na.rm)
      e_r <- ref_fun(actual, na.rm = na.rm)

      1 - (e_p / (e_r * 2))
    }
  )
}


#' Standardized accuracy of the MAE based on the MAD.
#'
#' @export
#' @rdname staccuracy
#'
sa_mae_mad <- standardized_accuracy(mae, mad)

#' Standardized accuracy of the winsorized MAE based on the MAD.
#'
#' @export
#' @rdname staccuracy
#'
sa_wmae_mad <- standardized_accuracy(win_mae, mad)

#' Standardized accuracy of the RMSE based on the standard deviation.
#'
#' @export
#' @rdname staccuracy
#'
sa_rmse_sd <- standardized_accuracy(rmse, stats::sd)

#' Standardized accuracy of the winsorized RMSE based on the standard deviation.
#'
#' @export
#' @rdname staccuracy
#'
sa_wrmse_sd <- standardized_accuracy(win_rmse, stats::sd)
