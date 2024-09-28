# We added three simple functions to generate common model evaluation measures: `rmse` for the root mean squared error, `mae` for the mean absolute error, and `mad` for the mean absolute deviation.



# Calculate the error (or deviation) between two numeric vectors
#
# Not exported. Returns the error (or deviation) between two numeric vectors. This is a utility
# function for other functions that need to calculate such error or deviation
# because this function validates the inputs and handles the `na.rm` instruction.
#
# @param actual numeric vector. Actual (true) values from a dataset.
# @param pred numeric vector. Predictions corresponding to each respective
# element in `actual`.
# @param na.rm single logical. TRUE if missing values should be removed; FALSE
# if they should be retained. If TRUE, then if any element of either actual or
# pred is missing, its paired element will be also removed.
#
# @returns Numeric vector of the same length as `actual` and `pred`. But if
# `na.rm = TRUE`, the vector will be shortened to omit any values where either
# actual or pred is `NA`.
#
error_vector <- function(actual, pred, na.rm = FALSE) {
  # Validate inputs
  validate(is.numeric(actual))
  validate(is.numeric(pred))
  validate(length(actual) == length(pred))
  validate(is_scalar_logical(na.rm) && !is.na(na.rm))

  error <- actual - pred

  if (na.rm) {
    error <- error[!is.na(error)]
  }

  return(error)
}



#' Mean absolute error (MAE)
#'
#' Returns the mean absolute error (MAE) of predicted values relative to the actual values.
#'
#' @export
#' @rdname reg-error
#'
#' @param actual numeric vector. Actual (true) values from a dataset.
#' @param pred numeric vector. Predictions corresponding to each respective
#' element in `actual`.
#' @param na.rm single logical. `TRUE` if missing values should be removed; `FALSE` if they should be retained. If `TRUE`, then if any element of either `actual`  or `pred` is missing, its paired element will be also removed.
#'
#' @returns MAE of `actual` and `pred`. If any value in `actual` or `pred` is
#' `NA` and `na.rm = FALSE`, then returns `NA`.
#'
mae <- function(actual, pred, na.rm = FALSE) {
  error_vector(actual, pred, na.rm) |>
    abs() |>
    mean()
}


#' Root mean squared error
#'
#' Returns the root mean squared error (RMSE) of predicted values relative to the actual values.
#'
#' @export
#' @rdname reg-error
#'
#' @param actual See documentation for [mae()]
#' @param pred See documentation for [mae()]
#' @param na.rm See documentation for [mae()]
#'
#' @returns RMSE of `actual` and `pred`. If any value in `actual` or `pred` is `NA` and `na.rm = FALSE`, returns `NA`.
#'
rmse <- function(actual, pred, na.rm = FALSE) {
  error_vector(actual, pred, na.rm) |>
    (`^`)(2) |>
    mean() |>
    sqrt()
}


#' Winsorize a numeric vector.
#'
#' Returns the input vector values with values less than or greater than the provided minimum or maximum replaced by the provided minimum or maximum, respectively.
#'
#' @export
#' @rdname win-error
#'
#' @param x numeric vector.
#' @param win_range numeric(2). A pair of single numeric values: the minimum and maximum allowable values for x, respectively.
#'
#' @returns Winsorized MAE of `actual` and `pred`. See mae() for details.
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


#' Winsorized mean absolute error (WinMAE)
#'
#' Returns the mean absolute error (MAE) with predictions winsorized within a specified range.
#'
#' @export
#' @rdname win-error
#'
#' @param actual numeric vector. Actual (true) values from a dataset.
#' @param pred numeric vector. Predictions corresponding to each respective
#' element in `actual`.
#' @param win_range numeric(2). The minimum and maximum allowable values for the `pred` predictions. Defaults to the minimum and maximum values of the provided `actual` values.
#' @param na.rm single logical. `TRUE` if missing values should be removed; `FALSE` if they should be retained. If `TRUE`, then if any element of either `actual`  or `pred` is missing, its paired element will be also removed.
#'
#' @returns Winsorized MAE of `actual` and `pred`. See mae() for details.
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


#' Mean absolute deviation
#'
#' Returns the mean absolute deviation (MAD) of values relative to their mean. This is useful as a default benchmark for the mean absolute error (MAE), as the standard deviation (SD) is a default benchmark for the root mean square error (RMSE).
#'
#' NOTE: This function name overrides `stats::mad()` (median absolute deviation relative to their median). To maintain the functionality of `stats::mad()`, specify the `version` argument.
#'
#' @export
#' @rdname reg-error
#'
#' @param x numeric vector. Values for which to calculate the mean absolute deviation.
#' @param na.rm logical(1). TRUE if missing values should be removed; FALSE if they should be retained.
#' @param version character(1). By default (`version = 'mean'`), `mad()` returns the mean absolute deviation (MAD) of values relative to their mean. If `version = 'median'`, it calls the `stats::mad()` function instead, the median absolute deviation relative to their median. Any other value gives an error.
#' @param ... Arguments to pass to `stats::mad()` if `version = 'median'`. See the `version` argument for details.
#'
#' @returns MAD of the `x` values. If any value of `x` is `NA` and `na.rm = FALSE`, returns `NA`.
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
