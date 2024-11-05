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
#' Because the distribution of staccuracies is uncertain (and indeed, different staccuracies likely have different distributions), bootstrapping is used to empirically estimate the distributions and calculate the p-values. See the return value description for details on what the function provides.
#'
#' @param actual numeric vector. The actual (true) labels.
#' @param preds named list of at least two numeric vectors. Each element is a vector of the same length as actual with predictions for each row corresponding to each element of actual. The names of the list elements should be the names of the models that produced each respective prediction; these names will be used to distinguish the results.
#' @param ... not used. Forces explicit naming of subsequent arguments.
#' @param sa list of functions. Each element is the unquoted name of a valid staccuracy function (see [staccuracy()] for the required function signature.) If an element is named, the name will be displayed as the value of the `sa` column of the result. Otherwise, the function name will be displayed. If NULL (default), staccuracy functions will be automatically selected based on the datatypes of actual and `preds`.
#' @param na.rm See documentation for [staccuracy()]
#' @param pct numeric with values from (0, 1). The percentage values on which the difference in staccuracies will be tested.
#' @param boot_alpha numeric(1) from 0 to 1. Alpha for percentile-based confidence interval range for the bootstrapped means; the bootstrap confidence intervals will be the lowest and highest `(1 - 0.05) / 2` percentiles. For example, if `boot_alpha = 0.05` (default), the intervals will be at the 2.5 and 97.5 percentiles.
#' @param boot_it positive integer(1). The number of bootstrap iterations.
#' @param seed integer(1). Random seed for the bootstrap sampling. Supply this between runs to assure identical results.
#'
#' @return tibble with staccuracy difference results:
#' * `staccuracy`: name of staccuracy measure
#' * `pred`, `type`: When `type` is 'pred', the `pred` column gives named element in the input `preds`. The row values give the staccuracy for that prediction.  When `type` is 'diff', the `pred` column is of the form 'model1-model2', where 'model1' and 'model2' are names from the input `preds`, which should be the names of each model that provided the predictions. The row values give the difference between staccuracies of model1 and model2.
#' * `lo`, `mean`, `hi`: The lower bound, mean, and upper bound of the bootstrapped staccuracy. The lower and upper bounds are confidence intervals specified by the input `boot_alpha`.
#' * `p__`: p-values that the staccuracies are at least the specified percentage difference or greater.  E.g., for the default input `pct = c(0.01, 0.02, 0.03, 0.04, 0.05)`, these columns would be `p01`, `p02`, `p03`, `p04`, and  `p05`. As they apply only to differences between staccuracies, they are `NA` for rows of `type` 'pred'. As an example of their meaning, if the `mean` difference for 'model1-model2' is 0.0832 with `p01` of 0.012 and `p02` of 0.035, then it means that 1.2% of bootstrapped staccuracies had a difference of model1 - model2 less than 0.01 and 3.5% were less than 0.02. (That is, 98.8% of differences were greater than 0.01 and 96.5% were greater than 0.02.)
#'
#' @export
#'
#' @examples
#' lm_attitude_all <- lm(rating ~ ., data = attitude)
#' lm_attitude__a <- lm(rating ~ . - advance, data = attitude)
#' lm_attitude__c <- lm(rating ~ . - complaints, data = attitude)
#'
#' sdf <- sa_diff(
#'   attitude$rating,
#'   list(
#'     all = predict(lm_attitude_all),
#'     madv = predict(lm_attitude__a),
#'     mcmp = predict(lm_attitude__c)
#'   ),
#'   boot_it = 10
#' )
#' sdf
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

  len <- length(actual)

  if (d_type == 'numeric') {
    sa <- list(
      `WinMAE on MAD` = sa_wmae_mad,
      `WinRMSE on SD` = sa_wrmse_sd
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
        1:len
      } else {  # bootstrap: sample len with replacement
        sample.int(len, replace = TRUE)
      }
    }),
    staccuracy = character(boot_it + 1),
    pred = character(boot_it + 1),
    val = double(boot_it + 1)
  )

  # Bootstrap the calculations of staccuracy
  sa_boot <-
    # Iteration 0 is the full sample
    map(0:boot_it, \(btit) {
      # Iterate across staccuracy measures
      imap(sa, \(it.sa, it.sa_name) {
        # Iterate across model predictions
        imap(preds, \(it.pred, it.pred_name) {
          tibble(
            it = btit,
            staccuracy = it.sa_name,
            pred = it.pred_name,
            val = it.sa(
              actual[boot_tbl$row_idxs[[btit+1]]],
              it.pred[boot_tbl$row_idxs[[btit+1]]],
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
    )

  if (boot_it != 0) {
    sa_boot <- sa_boot |>
      filter(.data$it != 0)
  }

  # fs (first,second): tbl of all possible combinations of differences between model staccuracies.
  # The difference is first - second.
  fs <- tidyr::expand_grid(
    first = names(preds),
    second = names(preds)
  ) |>
    filter(first != .data$second)

  # Remove duplicate difference pairs
  unique_fs <- purrr::map_lgl(1:nrow(fs), \(i.r) {
    (fs[i.r, 'first'] %+% '|' %+% fs[i.r, 'second']) %notin%
      (pull(fs[1:(i.r-1), 'second']) %+% '|' %+% pull(fs[1:(i.r-1), 'first']))
  })
  fs <- fs[unique_fs, ]

  # Calculate the differences for each pair of model staccuracies
  for (i.row in 1:nrow(fs)) {
    #sa_boost$`first-second` <- sa_boot$first - sa_boot$second
    sa_boot[[
      fs[[i.row, 'first']] %+% '-' %+% fs[[i.row, 'second']]
    ]] <-
      sa_boot[[fs[[i.row, 'first']]]] - sa_boot[[fs[[i.row, 'second']]]]
  }

  # Create names for the percent difference columns
  names_pct <- 'p' %+%
    stringr::str_pad(pct * 100, width = 2, pad = '0')

  # Generate expressions for p-values each percent difference threshold
  p_pct_exprs <- map(pct, ~ expr(sum(value < !!.x) / boot_it))
  names(p_pct_exprs) <- names_pct

  # Summarize the bootstrapped staccuracies and differences
  sa_tbl <- sa_boot |>
    # Pivot long for easier summarization
    tidyr::pivot_longer(
      !c('it', 'staccuracy'),
      names_to = 'pred'
    ) |>
    summarize(
      .by = c('staccuracy', 'pred'),
      lo   = stats::quantile(.data$value, boot_alpha / 2),
      mean = mean(.data$value),
      hi   = stats::quantile(.data$value, 1 - (boot_alpha / 2)),
      # Summarize p-values for requested percent thresholds
      !!!p_pct_exprs,
    ) |>
    mutate(
      type = if_else(stringr::str_detect(.data$pred, '-'), 'diff', 'pred'),
      # Delete p-value differences for actual staccuracies; they are irrelevant here
      across(
        all_of(names_pct),
        ~ if_else(type == 'diff', .x, NA)
      )
    ) |>
    select('staccuracy', 'pred', 'type', everything())


  return(sa_tbl)
}
