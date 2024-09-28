# test-classification.R



test_that("aucroc function works correctly", {
  set.seed(0)

  a <- sample(c(TRUE, FALSE), 50, replace = TRUE)
  p <- runif(50) |> round(2)
  p[c(7, 8, 22, 35, 40, 41)] <- 0.5
  expect_snapshot(aucroc(a, p))
  # aucroc(a, p)$auc
  # yardstick::roc_auc_vec(factor(a), p)
  # pROC::roc(a, p) |> pROC::auc()
})
