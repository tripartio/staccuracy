# test-staccuracy.R



test_that("sa_diff() works correctly", {
  lm_attitude_all <- lm(rating ~ ., data = attitude)
  lm_attitude__a <- lm(rating ~ . - advance, data = attitude)
  lm_attitude__c <- lm(rating ~ . - complaints, data = attitude)

  expect_equal(
    sa_mae_mad(
      attitude$rating,
      predict(lm_attitude_all)
    ) |>
      round(2),
    0.72
  )
  expect_equal(
    sa_wmae_mad(
      attitude$rating,
      predict(lm_attitude__c)
    ) |>
      round(2),
    0.64
  )
  expect_equal(
    sa_rmse_sd(
      attitude$rating,
      predict(lm_attitude_all)
    ) |>
      round(2),
    0.75
  )
  expect_equal(
    sa_wrmse_sd(
      attitude$rating,
      predict(lm_attitude__c)
    ) |>
      round(2),
    0.68
  )

  expect_snapshot(
    sa_diff(
      attitude$rating,
      list(
        all = predict(lm_attitude_all),
        madv = predict(lm_attitude__a),
        mcmp = predict(lm_attitude__c)
      ),
      boot_it = 10
    )
  )
})
