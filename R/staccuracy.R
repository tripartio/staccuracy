# We added three simple functions to generate common model evaluation measures: `rmse` for the root mean squared error, `mae` for the mean absolute error, and `mad` for the mean absolute deviation.


# staccuracy ------------------

#' Create standardized accuracy (staccuracy) functions.
#'
#' @export
#' @rdname staccuracy
#'
#' @param error_fun function. The unquoted function name of the function that calculates error (or accuracy) measure. This function must be of the signature `error_fun(actual, pred, na.rm = FALSE)` where `actual` and `pred` are required numeric vectors of the true (actual) labels and the predicted estimates, respectively; `na.rm` is an optional scalar logical that indicates if NA values should be removed (`TRUE`) or not (`FALSE`, default).
#' @param ref_fun function. The unquoted function name of the function that calculates the reference error, accuracy, or deviation measure. This function must be of the signature `ref_fun(actual, na.rm = FALSE)` where `actual` and `na.rm` have the same meanings as described for `error_fun`.
#'
#' @returns Function with signature `function(actual, pred, na.rm = FALSE)` whose arguments have the same meanings as  described for the `error_fun` input argument.
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

sa_mae_mad <- standardized_accuracy(mae, mad)

sa_wmae_mad <- standardized_accuracy(win_mae, mad)

sa_rmse_sd <- standardized_accuracy(rmse, stats::sd)

sa_wrmse_sd <- standardized_accuracy(win_rmse, stats::sd)
