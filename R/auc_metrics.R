#' Calculate Partial and complete Area Under the Curve (AUC) Metrics
#'
#' Computes partial AUC ratios between model predictions and random curves at a specified threshold,
#' with options for sampling and iterations. Handles both numeric vectors and SpatRaster inputs.
#'
#' @param test_prediction Numeric vector of test prediction values (e.g., model outputs)
#' @param prediction Numeric vector or SpatRaster object containing prediction values
#' @param threshold Percentage threshold for partial AUC calculation (default = 5)
#' @param sample_percentage Percentage of test data to sample (default = 50)
#' @param iterations Number of iterations for random curve generation (default = 500)
#' @param compute_full_auc Logical. If TRUE, the complete AUC values will be computed
#'
#' @return A list containing:
#' \itemize{
#'   \item If input has no variability: List with NA values for AUC metrics
#'   \item Otherwise: Matrix of AUC results.
#' }
#'
#' @details
#' The function calculates partial AUC ratios by:
#' \enumerate{
#'   \item Validating input types and completeness
#'   \item Handling NA values and SpatRaster conversion
#'   \item Checking for prediction variability
#'   \item Computing AUC metrics using optimized C++ code
#' }
#'
#' When prediction values have no variability (all equal), the function returns NA values with a warning.
#'
#' @examples
#' # With numeric vectors
#' test_data <- rnorm(100)
#' pred_data <- rnorm(100)
#' result <- fpROC::auc_metrics(test_prediction = test_data, prediction = pred_data)
#'
#' # With SpatRaster
#' library(terra)
#' r <- terra::rast(ncol=10, nrow=10)
#' values(r) <- rnorm(ncell(r))
#' result <- auc_metrics(test_prediction = test_data, prediction = r)
#'
#' @export
#' @import RcppArmadillo
#' @importFrom stats na.omit
#' @importFrom RcppParallel RcppParallelLibs
#' @useDynLib fpROC, .registration=TRUE
auc_metrics <-function (test_prediction, prediction, threshold = 5, sample_percentage = 50,
                     iterations = 500,compute_full_auc = TRUE) {

  if (missing(prediction) || missing(test_prediction)) {
    stop("Both 'prediction' and 'test_prediction' are required")
  }

  if (!inherits(test_prediction, "numeric")) {
    stop("'test_prediction' must be numeric")
  }

  # Handle SpatRaster input
  if (inherits(prediction, "SpatRaster")) {
    prediction <- terra::values(prediction, na.rm = TRUE)
  } else if (!inherits(prediction, "numeric")) {
    stop("'prediction' must be numeric or SpatRaster")
  }

  prediction <- stats::na.omit(prediction)
  test_prediction <- stats::na.omit(test_prediction)

  # Check for variability
  if (diff(range(prediction)) == 0) {
    warning("No variability in predictions, returning NA")
    return(list(
      pROC_summary = c(
        AUC_ratio = NA,
        p_value = NA
      ),
      pROC_results = c(
        Model_AUC = NA,
        Random_AUC = NA,
        AUC_ratio = NA
      )
    ))
  }
  # ----------------------------------------------------------------------------
  # C++ functions
  auc_metr <- fpROC::auc_parallel(
    test_prediction = test_prediction,
    prediction = prediction,
    threshold = threshold,
    sample_percentage = sample_percentage,
    iterations = iterations,compute_full_auc = compute_full_auc
  )


  summ_auc_metrics <- fpROC::summarize_auc_results(auc_metr,compute_full_auc)
  # ----------------------------------------------------------------------------
  colnames(auc_metr) <- c("Model_full_auc",
                             "Model_partial_AUC",
                             "Random_curve_partial_AUC",
                             "AUC_ratio")
  colnames(summ_auc_metrics) <- c("Mean_Model_full_auc",
                                  paste0("Mean_Model_partial_AUC_at_",
                                         threshold,"_percent"),
                                  "Mean_Random_curve_partial_AUC",
                                  "Mean_AUC_ratio",
                                  "pval_pROC")

  return(list(summary = summ_auc_metrics,
         proc_results = auc_metr))

}

