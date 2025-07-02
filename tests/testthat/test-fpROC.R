library(testthat)
library(fpROC)
testthat::test_that("pROC metrics, pAUC ratios between model predictions and random", {
  test_data <- abs(rnorm(100))
  pred_data <- abs(rnorm(100))
  result <- fpROC::auc_metrics(test_prediction = test_data,
                               prediction = pred_data,
                               compute_full_auc = TRUE)


  testthat::expect_match(class(result$summary)[1],"matrix")
  testthat::expect_match(class(result$proc_results)[1],"matrix")
})


testthat::test_that("pROC metrics, pAUC ratios between raster model predictions and random", {
  test_data <- rnorm(100)
  r <- terra::rast(ncol=10, nrow=10)
  terra::values(r) <- rnorm(terra::ncell(r))
  result <-  fpROC::auc_metrics(test_prediction = test_data, prediction = r)
  testthat::expect_match(class(result$summary)[1],"matrix")
  testthat::expect_match(class(result$proc_results)[1],"matrix")
})


testthat::test_that("Summary of AUC and pAUC results",{
  # Basic usage with random data
  set.seed(123)
  train_pred <- runif(1000)   # Training predictions
  test_pred <- runif(500)     # Test predictions

  # Compute only partial AUC metrics (500 iterations)
  results <- fpROC::auc_parallel(test_pred, train_pred,
                                 threshold = 5.0,
                                 iterations = 100)  # Reduced for example



  # Summarize results (assume complete AUC was not computed)
  summary <- fpROC::summarize_auc_results(results, has_complete_auc = FALSE)
  testthat::expect_match(class(summary)[1],"matrix")
})

testthat::test_that("AUC computation",{
  x <- c(0, 0.5, 1, 1.5, 2)
  y <- c(0, 0.7, 0.9, 0.95, 1)
  auc <- fpROC::trap_roc(x, y)  # Returns AUC
  testthat::expect_match(class(auc),"numeric")


})

testthat::test_that("No variability in model preds",{
  set.seed(123)
  train_pred <- runif(100)   # Training predictions
  test_pred <- train_pred     # Test predictions


  testthat::expect_error(fpROC::auc_metrics(
                                 threshold = 5.0,
                                 iterations = 100))  # Reduced for example
  testthat::expect_error(fpROC::auc_metrics(test_prediction = "1",
                                             prediction = train_pred,
                                             threshold = 5.0,
                                             iterations = 100))  #
  testthat::expect_error(fpROC::auc_metrics(test_prediction = 1,
                                             prediction = "1",
                                             threshold = 5.0,
                                             iterations = 100))  #

  testthat::expect_warning(fpROC::auc_metrics(test_prediction = 1,
                                            prediction = rep(1,10),
                                            threshold = 5.0,
                                            iterations = 100))  #

})
