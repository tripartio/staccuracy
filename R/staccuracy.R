# staccuracy() ----------------------

#' Standardized accuracy (staccuracy) functions.
#'
#' @export
#' @rdname staccuracy
#'
#' @description
#' Standardized accuracy (staccuracy) represents error or accuracy measures on a scale where 1 or 100% means perfect prediction and 0.5 or 50% is a reference comparison of some specified standard performance. Higher than 0.5 is better than the reference and below 0.5 is worse. 0 might or might not have a special meaning; sometimes negative scores are possible, but these often indicate modelling errors.
#'
#' The core function is `staccuracy()`, which receives as input a generic error function and a reference function against which to compare the error function performance. In addition, the following recommended staccuracy functions are provided:
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
#' The core function `staccuracy()` receives as input a generic error function and a reference function against which to compare the error function's performance. These input functions must have the following signatures (see the argument specifications for details of the arguments):
#' * `error_fun`: `function(actual, pred, na.rm = na.rm)`; the output must be a scalar numeric (that is, a single number).
#' * `error_fun`: `function(actual, pred, na.rm = na.rm)`; the output must be a scalar numeric (that is, a single number).
#'
#' @returns
#' `staccuracy()` returns a function with signature `function(actual, pred, na.rm = FALSE)` that receives an `actual` and a `pred` vector as inputs and returns the staccuracy of the originally input error function based on the input reference function.
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
#' my_mae_vs_mad_sa <- staccuracy(mae, mad)
#'
#' # Now use it
#' my_mae_vs_mad_sa(actual_1, predicted_1)
#'
#' # That's 94.2% standardized accuracy compared to the MAD. Pretty good!
#'
#'
staccuracy <- function(
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


# Convenience functions -----------------------

#' Standardized accuracy of the MAE based on the MAD.
#'
#' @export
#' @rdname staccuracy
#'
sa_mae_mad <- staccuracy(mae, mad)

#' Standardized accuracy of the winsorized MAE based on the MAD.
#'
#' @export
#' @rdname staccuracy
#'
sa_wmae_mad <- staccuracy(win_mae, mad)

#' Standardized accuracy of the RMSE based on the standard deviation.
#'
#' @export
#' @rdname staccuracy
#'
sa_rmse_sd <- staccuracy(rmse, stats::sd)

#' Standardized accuracy of the winsorized RMSE based on the standard deviation.
#'
#' @export
#' @rdname staccuracy
#'
sa_wrmse_sd <- staccuracy(win_rmse, stats::sd)




# sa_diff() ------------------------

#' Statistical tests for the differences between standardized accuracies (staccuracies)
#'
#' Because the distribution of staccuracies is uncertain (and indeed, different staccuracies likely have different distributions), bootstrapping is used to empirically estimate the distributions and calculate the p-values.
#  But what test is appropriate? I don't think the t-test (a test of differences in means) is appropriate to test the difference between staccuracies. At the very simplest, I could bootstrap the differences and then compare confidence intervals. But ideally, I should work with a statistician to develop and analytical test of differences between staccuracies. The challenge is that different staccuracies might have different distributions. So, bootstrapping would be a simple, useful function. In the meantime, I could use univariateML to observe the distributions and thus gain an intuition. Parallelization should be an option to speed things up.
#'
#'
#'
#'
#' @param actual numeric vector. The actual (true) labels.
#' @param preds named list of at least two numeric vectors. Each element is a vector of the same length as actual with predictions for each row corresponding to each element of actual. The names of the list elements will be used for the columns of the result.
#' @param ... not used. Forces explicit naming of subsequent arguments.
#' @param na.rm See documentation for [staccuracy()]
#' @param sa list of functions. Each element is the unquoted name of a staccuracy function. If an element is named, the name will be displayed as the value of the `sa` column of the result. Otherwise, the function name will be displayed. If NULL (default), staccuracy functions will be automatically selected based on the datatypes of actual and `preds`.
#' @param pct numeric with values from (0, 1). The percentage values on which the difference in staccuracies will be tested.
#' @param boot_alpha numeric(1) from 0 to 1. Alpha for percentile-based confidence interval range for the bootstrapped means; the bootstrap confidence intervals will be the lowest and highest `(1 - 0.05) / 2` percentiles. For example, if `boot_alpha = 0.05` (default), the intervals will be at the 2.5 and 97.5 percentiles.
#' @param boot_it positive integer(1). The number of bootstrap iterations.
#' @param seed integer(1). Random seed for the bootstrap sampling. Supply this between runs to assure identical results.
#'
#' @return tibble. Columns are `sa` (name of staccuracy measure), then a column for each named element in `preds` (staccuracy for that prediction), then a column for each element of `pct` (p-value; percentage of iterations where the difference between staccuracy in the pair of `preds` is equal or greater than the `pct` value). E.g., for the default `pct = c(0.01, 0.02, 0.03, 0.04, 0.05)`, these columns would be `p_1`, `p_2`, `p_3`, `p_4`, and  `p_5`. When there are more than two predictions, then each row will compare only two at a time.
#'
#' @export
#'
#' @examples
#' lm_attitude_all <- lm(rating ~ ., data = attitude)
#' lm_attitude__c <- lm(rating ~ . - complaints, data = attitude)
#'
#' sa_diff(
#'   attitude$rating,
#'   list(
#'     all = predict(lm_attitude_all),
#'     madv = predict(lm_attitude__c)
#'   ),
#'   boot_it = 10
#' )
#'
sa_diff <- function(
    actual,
    preds,
    ...,
    na.rm = FALSE,
    sa = NULL,
    pct = c(0.01, 0.02, 0.03, 0.04, 0.05),
    boot_alpha = 0.05,
    boot_it = 1000,
    seed = 0
) {
  d_type <- var_type(actual)

  lgth <- length(actual)

  if (d_type == 'numeric') {
    sa <- list(
      `Staccuracy WinMAE on MAD` = sa_wmae_mad,
      `Staccuarcy WinRMSE on SD` = sa_wrmse_sd
    )
  }

  # Create bootstrap tbl
  original_seed <- .Random.seed
  on.exit(set.seed(original_seed))
  set.seed(seed)

  boot_tbl <- tibble(
    # it: bootstrap iteration number. Row 0 is the full dataset without bootstrapping
    it = 0:boot_it,
    # row_idxs: row indices of each bootstrap sample. Store just the indices rather than duplicating the entire dataset multiple times.
    row_idxs = map(0:boot_it, \(it.bt) {
      if (it.bt == 0) {  # row 0 is the full dataset without bootstrapping
        1:lgth
      } else {  # bootstrap: sample lgth with replacement
        sample.int(lgth, replace = TRUE)
      }
    }),
    staccuracy = character(boot_it + 1),
    pred = character(boot_it + 1),
    val = double(boot_it + 1)
  )


  sa_boot <-
    map(0:boot_it, \(it) {
      imap(sa, \(it.sa, it.sa_name) {
        imap(preds, \(it.pred, it.pred_name) {
          tibble(
            it = it,
            staccuracy = it.sa_name,
            pred = it.pred_name,
            val = it.sa(
              actual[boot_tbl$row_idxs[[it+1]]],
              it.pred[boot_tbl$row_idxs[[it+1]]],
              na.rm = FALSE
            )
          )
        }) |>
          bind_rows()
      }) |>
        bind_rows()
    }) |>
    bind_rows() |>
    tidyr::pivot_wider(
      names_from = 'pred',
      values_from = 'val'
    ) |>
    # Add the difference between the measures
    rowwise() |>
    mutate(
      diff = max(c_across(where(is.double))) - min(c_across(where(is.double)))
    ) |>
    ungroup()

  # Staccuracies of the full vectors
  sa_full <- sa_boot |>
    filter(.data$it == 0) |>
    select(-'it')

  # browser()

  # Summarize the bootstrapped staccuracies without the full vectors
  sa_tbl <- sa_boot |>
    filter(.data$it != 0) |>
    select(-'it') |>
    summarize(
      .by = c(staccuracy),
      across(
        where(is.numeric),
        list(
          lo = ~ quantile(.x, boot_alpha / 2),
          mn = mean,
          hi = ~ quantile(.x, 1 - (boot_alpha / 2))
        )
      ),
      across(
        diff,
        map(pct, \(it.p) {
          \(.x) {sum(.x < it.p) / !!boot_it}
        }) |>
          set_names(
            formatC(pct * 100, width = 2, flag = '0')
          ),
        .names = 'p_{.fn}'
      )
      # p_1 = sum(diff < 0.01) / boot_it,
      # p_2 = sum(diff < 0.02) / boot_it,
      # p_3 = sum(diff < 0.03) / boot_it,
      # p_4 = sum(diff < 0.04) / boot_it,
      # p_5 = sum(diff < 0.05) / boot_it,
    )

  return(sa_tbl)
}
