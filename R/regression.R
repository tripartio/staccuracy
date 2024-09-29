# Regression error and deviation measures -------------------

#' Regression error and deviation measures
#'
#'
#' @rdname reg-error
#'
#' @description
#' These are standard error and deviation measures for numeric data. "Deviation" means the natural variation of the values of a numeric vector around its central tendency (usually the mean or median). "Error" means the average discrepancy between the actual values of a numeric vector and its predicted values.
#'
#' @details
#' **Mean absolute deviation (MAD)**
#'
#' `mad()` returns the mean absolute deviation (MAD) of values relative to their mean. This is useful as a default benchmark for the mean absolute error (MAE), as the standard deviation (SD) is a default benchmark for the root mean square error (RMSE).
#'
#' **NOTE:** This function name overrides `stats::mad()` (median absolute deviation relative to their median). To maintain the functionality of `stats::mad()`, specify the `version` argument.
#'
#'
#' @param actual numeric vector. Actual (true) values of target outcome data.
#' @param pred numeric vector. Predictions corresponding to each respective element in `actual`.
#' @param na.rm logical(1). `TRUE` if missing values should be removed; `FALSE` if they should be retained. If `TRUE`, then if any element of either `actual` or `pred` is missing, its paired element will be also removed.
#'
#' @returns
#' In all cases, if any value in `actual` or `pred` is `NA` and `na.rm = FALSE`, then the function returns `NA`.
#'
#'
#' @examples
#' a <- c(3, 5, 2, 7, 9, 4, 6, 8, 1, 10)
#' p <- c(2.5, 5.5, 2, 6.5, 9.5, 3.5, 6, 7.5, 1.5, 9.5)


#' @export
# @rdname reg-error
#'
#' @returns `mae()` returns the mean absolute error (MAE) of predicted values `pred` compared to the `actual` values.
#'
#' @examples
#' mae(a, p)
#'
mae <- function(actual, pred, na.rm = FALSE) {
  error_vector(actual, pred, na.rm) |>
    abs() |>
    mean()
}


#' @export
#' @rdname reg-error
#'
#' @returns `rmse()` returns the root mean squared error (RMSE) of predicted values `pred` compared to the `actual` values.
#'
#' @examples
#' rmse(a, p)
#'
rmse <- function(actual, pred, na.rm = FALSE) {
  error_vector(actual, pred, na.rm) |>
    (`^`)(2) |>
    mean() |>
    sqrt()
}



#' @export
#' @rdname reg-error
#'
#' @param x numeric vector. Values for which to calculate the MAD.
#' @param version character(1). By default (`version = 'mean'`), `mad()` returns the mean absolute deviation (MAD) of values relative to their mean. If `version = 'median'`, it calls the `stats::mad()` function instead, the median absolute deviation relative to their median (MedAD, sometimes also called MAD). Any other value gives an error. See details.
#' @param ... Arguments to pass to `stats::mad()` if `version = 'median'`. See the `version` argument for details.
#'
#' @returns `mad()` returns either the mean absolute deviation (MAD) of values relative to their mean (default) or the median absolute deviation relative to their median. See details.
#'
#' @examples
#' mad(a)
#'
mad <- function(x, na.rm = FALSE, version = 'mean', ...) {
  validate(
    version %in% c('mean', 'median'),
    msg = c(
      "{.var version} must be either {.val mean} or {.val median}",
      "x" = 'It is instead {version}.'
    )
  )

  if (version == 'mean') {
    return(
      # MAD is MAE of values around their mean
      mae(
        actual = rep(mean(x, na.rm = na.rm), length(x)),
        pred = x,
        na.rm = na.rm
      )
    )
  }

  else if (version == 'median') {
    return(stats::mad(x = x, na.rm = na.rm, ...))
  }
}




#' @noRd
#' @keywords internal
# Calculate the error (or deviation) between two numeric vectors
#
# Not exported. Returns the error (or deviation) between two numeric vectors. This is a utility function for other functions that need to calculate such error or deviation because this function validates the inputs and handles the `na.rm` instruction.
#'
#' @returns Numeric vector of the differences between `actual` and `pred`; length is the same as `actual` and `pred`. But if `na.rm = TRUE`, the vector will be shortened to omit any values where either actual or pred is `NA`.
#'
#' @examples
#' a <- c(3, 5, 2, 7, 9, 4, 6, 8, 1, 10)
#' p <- c(2.5, 5.5, 2, 6.5, 9.5, 3.5, 6, 7.5, 1.5, 9.5)
#' error_vector(a, p)
#'
error_vector <- function(actual, pred, na.rm = FALSE) {
  # Validate inputs
  validate(is.numeric(actual))
  validate(is.numeric(pred))
  validate(length(actual) == length(pred))
  validate(rlang::is_scalar_logical(na.rm) && !is.na(na.rm))

  error <- actual - pred

  if (na.rm) {
    error <- error[!is.na(error)]
  }

  return(error)
}



# Winsorization measures ------------------

#'  Winsorize a numeric vector
#'
#' @description
#' Winsorization means truncating the extremes of a numeric range by replacing extreme values with a predetermined minimum and maximum. `winsorize()` returns the input vector values with values less than or greater than the provided minimum or maximum replaced by the provided minimum or maximum, respectively.
#'
#' `win_mae()` and `win_rmse()` return MAE and RMSE respectively with winsorized predictions. The fundamental idea underlying the winsorization of predictions is that if the actual data has well-defined bounds, then models should not be penalized for being overzealous in predicting beyond the extremes of the data. Models that are overzealous in the boundaries might sometimes be superior within normal ranges; the extremes can be easily corrected by winsorization.
#'
#' @export
#' @rdname win-error
#'
#' @param x numeric vector.
#' @param win_range numeric(2). The minimum and maximum allowable values for the `pred` predictions or for `x`. For functions with `pred`, `win_range` defaults to the minimum and maximum values of the provided `actual` values. For functions with `x`, there is no default.
#'
#' @returns `winsorize()` returns a winsorized vector.
#'
#' @examples
#' a <- c(3, 5, 2, 7, 9, 4, 6, 8, 2, 10)
#' p <- c(2.5, 5.5, 1.5, 6.5, 10.5, 3.5, 6, 7.5, 0.5, 11.5)
#'
#' a  # the original data
#' winsorize(a, c(2, 8))  # a winsorized on defined boundaries
#'
winsorize <- function(
    x,
    win_range
) {
  validate(is.numeric(x))
  validate(
    is.numeric(win_range),
    length(win_range) == 2,
    !any(is.na(win_range)),
    win_range[1] <= win_range[2],
    msg = cli_alert_danger(
      '{.var win_range} must be a numeric vector with exactly two non-missing values: a minimum and a larger maximum'
    )
  )

  return(
    case_when(
      x < win_range[1] ~ win_range[1],
      x > win_range[2] ~ win_range[2],
      .default = x
    )
  )
}


#' @export
#' @rdname win-error
#'
#' @param actual numeric vector. Actual (true) values of target outcome data.
#' @param pred numeric vector. Predictions corresponding to each respective element in `actual`.
#' @param na.rm logical(1). `TRUE` if missing values should be removed; `FALSE` if they should be retained. If `TRUE`, then if any element of either `actual` or `pred` is missing, its paired element will be also removed.
#'
#' @returns `win_mae()` returns the mean absolute error (MAE) of winsorized predicted values `pred` compared to the `actual` values. See `mae()` for details.
#'
#' @examples
#' # range of the original data
#' a
#' range(a)
#'
#' # some overzealous predictions
#' p
#' range(p)
#'
#' # MAE penalizes overzealous predictions
#' mae(a, p)
#'
#' # Winsorized MAE forgives overzealous predictions
#' win_mae(a, p)
#'
win_mae <- function(
    actual,
    pred,
    win_range = range(actual),
    na.rm = FALSE
) {
  mae(
    actual,
    winsorize(pred, win_range),
    na.rm
  )
}

#' @export
#' @rdname win-error
#'
#' @returns `win_rmse()` returns the root mean squared error (RMSE) of winsorized predicted values `pred` compared to the `actual` values. See `rmse()` for details.
#'
#' @examples
#' # RMSE penalizes overzealous predictions
#' rmse(a, p)
#'
#' # Winsorized RMSE forgives overzealous predictions
#' win_rmse(a, p)
#'
win_rmse <- function(
    actual,
    pred,
    win_range = range(actual),
    na.rm = FALSE
) {
  rmse(
    actual,
    winsorize(pred, win_range),
    na.rm
  )
}

