#include <RcppArmadillo.h>
#ifdef _OPENMP
#include <omp.h>
#endif

// [[Rcpp::plugins(openmp)]]
// [[Rcpp::depends(RcppArmadillo)]]
// [[Rcpp::plugins(cpp11)]]

using namespace arma;
using namespace Rcpp;

//' Calculate Area Under Curve (AUC) using trapezoidal rule
//'
//' @description Computes the area under a curve using the trapezoidal rule of numerical integration.
//'
//' @param x Numeric vector (arma::vec) of x-coordinates (should be sorted in increasing order)
//' @param y Numeric vector (arma::vec) of y-coordinates corresponding to x-coordinates
//'
//' @return A numerical value representing the computed area under the curve as a double precision value.
//'
//' @details
//' The trapezoidal rule approximates the area under the curve by dividing it into trapezoids.
//' For each pair of adjacent points (x[i], y[i]) and (x[i+1], y[i+1]), it calculates the area of the trapezoid formed.
//' The total AUC is the sum of all these individual trapezoid areas.
//'
//' Special cases:
//' - Returns 0 if there are fewer than 2 points (no area can be calculated)
//' - Handles both increasing and decreasing x values (though typically x should be increasing for ROC curves)
//'
//' @examples
//' # R code example:
//' x <- c(0, 0.5, 1, 1.5, 2)
//' y <- c(0, 0.7, 0.9, 0.95, 1)
//' trap_roc(x, y)
//'
//' @seealso \code{\link{integrate}} for R's built-in integration functions
//' @export
// [[Rcpp::export]]
double trap_roc(const arma::vec& x, const arma::vec& y) {
   const size_t n = x.n_elem;
   if (n < 2) return 0.0;

   double sum = 0.0;
   const double* x_ptr = x.memptr();
   const double* y_ptr = y.memptr();

   for(size_t i = 1; i < n; ++i) {
     sum += (x_ptr[i] - x_ptr[i-1]) * (y_ptr[i] + y_ptr[i-1]);
   }

   return 0.5 * sum;
 }

//' Compute Binned Classification Matrix for AUC Calculation
//'
//' @description Preprocesses prediction vectors by binning values and creating a matrix structure
//'              optimized for fast AUC computation. This function:
//'              1. Cleans and combines prediction vectors
//'              2. Performs range-based binning
//'              3. Computes background suitability data histogram
//'              4. Generates test prediction comparison matrix
//'
//' @param test_prediction Numeric vector (arma::vec) of prediction values for test data
//' @param prediction Numeric vector (arma::vec) of prediction values for background suitability data
//' @param n_bins Integer specifying number of bins to use for discretization (default = 1000)
//'
//' @return A numeric matrix where:
//'          - Rows correspond to test observations
//'          - Columns correspond to bins
//'          - Values represent bin indices in descending order (n_bins, n_bins-1, ..., 1)
//'
//' @details
//' This function prepares data for efficient AUC computation by:
//' 1. Cleaning: Removes non-finite values from background suitability predictions
//' 2. Combining: Merges background suitability and test predictions
//' 3. Binning: Discretizes values into [1, n_bins] range using:
//'        bin = floor((value - min) * (n_bins-1)/range) + 1
//' 4. Histogram: Computes background suitability data distribution across bins (parallelized)
//' 5. Matrix construction: Creates comparison matrix for test observations
//'
//' The output matrix enables fast computation of classification metrics by allowing
//' vectorized comparisons during AUC calculation.
//' This is the same rationale as the implementation applied to ntbox R package.
//'
//' @section Parallelization:
//' Uses OpenMP parallelization for:
//' - Binning computations
//' - Histogram counting
//'
//' @section Input Requirements:
//' - Input vectors must be non-empty
//' - background suitability predictions must contain at least one finite value
//' - Prediction range must be > 0 (non-constant values)
//'
//' @examples
//' # R usage example:
//' bg_pred <- runif(1000)
//' test_pred <- runif(500)
//' bin_matrix <- bigclass_matrix(test_pred, bg_pred, n_bins = 500)
//' dim(bin_matrix)  # 500 rows x 500 columns
//'
//' @seealso \code{\link{auc_parallel}} for the main AUC computation function
arma::mat bigclass_matrix(const arma::vec& test_prediction,
                           const arma::vec& prediction,
                           const int n_bins = 1000) {
  // Input validation
  if (prediction.n_elem == 0 || test_prediction.n_elem == 0) {
    Rcpp::stop("Input vectors cannot be empty");
  }

  arma::uvec non_na = arma::find_finite(prediction);
  if (non_na.n_elem == 0) {
    Rcpp::stop("No finite values in prediction vector");
  }

  const arma::vec pred_clean = prediction.elem(non_na);
  const int nprediction = pred_clean.n_elem;

  // Combine vectors
  arma::vec combined_pred = arma::join_cols(pred_clean, test_prediction);

  // Vectorized binning with bounds checking
  const double pred_min = combined_pred.min();
  const double pred_max = combined_pred.max();
  const double range = pred_max - pred_min;

  if (range <= std::numeric_limits<double>::epsilon()) {
    Rcpp::stop("All prediction values are identical");
  }

  const double scale = (n_bins - 1.0) / range;

  // Pre-allocate and compute binned values in one pass
  arma::vec pred_binned(combined_pred.n_elem);
  const double* pred_ptr = combined_pred.memptr();
  double* binned_ptr = pred_binned.memptr();

#pragma omp parallel for
  for(size_t i = 0; i < combined_pred.n_elem; ++i) {
    double val = (pred_ptr[i] - pred_min) * scale;
    binned_ptr[i] = std::floor(val) + 1.0;
  }

  // Split again
  arma::vec test_pred_binned = pred_binned.subvec(nprediction, pred_binned.n_elem - 1);
  arma::vec bg_pred_binned = pred_binned.subvec(0, nprediction - 1);

  // Parallel histogram counting with reduction
  arma::ivec counts(n_bins, arma::fill::zeros);
  const double* bg_ptr = bg_pred_binned.memptr();
  const size_t n_bg = bg_pred_binned.n_elem;

#pragma omp parallel for
  for (size_t i = 0; i < n_bg; ++i) {
    int bin = static_cast<int>(bg_ptr[i]) - 1;
    if (bin >= 0 && bin < n_bins) {
#pragma omp atomic
      counts[bin]++;
    }
  }
  const int n_samp = test_pred_binned.n_elem;
  arma::mat big_classpixels(n_samp, n_bins);

  for (int i = 0; i < n_bins; ++i) {
    big_classpixels.col(i).fill(n_bins - i);
  }

  return big_classpixels;
}

//' Compute AUC Metrics for single bootstrap iteration
//'
//' @description Calculates partial and complete AUC metrics for a single bootstrap sample.
//' This function is the computational core for bootstrap AUC estimation.
//'
//' @param big_classpixels Numeric matrix where:
//'          - Rows represent test observations
//'          - Columns represent bins
//'          - Values are bin indices (n_bins, n_bins-1, ... 1)
//' @param fractional_area Numeric vector of cumulative fractional areas (x-axis values for ROC)
//' @param test_prediction Numeric vector of binned test predictions (output from binning process)
//' @param n_samp Integer specifying number of test observations to sample
//' @param error_sens Double specifying sensitivity threshold for partial AUC (1 - error_rate)
//' @param compute_full_auc Boolean indicating whether to compute complete AUC
//'
//' @return A numeric matrix with 1 row and 4 columns containing:
//' \itemize{
//'   \item Column 1: Complete AUC (NA if compute_full_auc = FALSE)
//'   \item Column 2: Partial AUC for model (sensitivity > error_sens)
//'   \item Column 3: Partial AUC for random model (reference)
//'   \item Column 4: Ratio of model AUC to random AUC (model/reference)
//' }
//'
//' @details
//' The function performs these steps:
//' 1. Randomly samples test predictions (without replacement)
//' 2. Computes omission matrix by comparing bin indices with sampled predictions
//' 3. Calculates sensitivity as 1 - mean omission rate per bin
//' 4. Filters bins where sensitivity exceeds threshold
//' 5. Computes partial AUC for model and random reference
//' 6. Optionally computes complete AUC using all bins
//' 7. Calculates AUC ratio (model/reference)
//'
//' Special cases:
//' - Returns matrix of NAs if < 2 bins meet sensitivity threshold
//' - Returns zeros if either partial AUC is 0 (to prevent division by zero)
//'
//' @section Parallelization:
//' Uses OpenMP parallelization for:
//' - Omission matrix computation
//'
//' @section Algorithm Notes:
//' - Partial AUC focuses on high-sensitivity region (error_sens to 1.0)
//' - Random reference AUC is the theoretical AUC for uniform predictions
//' - Binning enables efficient vectorized comparisons
//'
//' @seealso \code{\link{auc_parallel}} for the main bootstrap function,
//'          \code{\link{trap_roc}} for AUC calculation method
arma::mat calc_aucDF_arma(
     const arma::mat& big_classpixels,
     const arma::vec& fractional_area,
     const arma::vec& test_prediction,
     int n_samp,
     double error_sens,
     bool compute_full_auc) {

   // Random sampling without replacement
   arma::uvec rowsID = arma::randperm(test_prediction.n_elem, n_samp);
   const arma::vec sampled_pred = test_prediction.elem(rowsID);

   // Vectorized omission matrix calculation
   arma::mat omission_matrix(big_classpixels.n_rows, big_classpixels.n_cols);

#pragma omp parallel for
   for (uword i = 0; i < big_classpixels.n_cols; ++i) {
     omission_matrix.col(i) = arma::conv_to<vec>::from(big_classpixels.col(i) > sampled_pred);
   }

   // Sensitivity calculation
   arma::vec sensibility = 1.0 - arma::mean(omission_matrix, 0).t();

   // Compute partial AUC
   arma::uvec keep_idx_partial = arma::find(sensibility > error_sens);
   if (keep_idx_partial.n_elem < 2) {
     arma::mat result(1, 4, arma::fill::value(NA_REAL));
     return result;
   }

   // Construct and sort partial AUC table
   arma::mat xyTable_partial = arma::join_horiz(
     fractional_area.elem(keep_idx_partial),
     sensibility.elem(keep_idx_partial)
   );
   xyTable_partial = xyTable_partial.rows(arma::sort_index(xyTable_partial.col(0)));

   // Compute AUCs
   const double auc_pmodel = trap_roc(xyTable_partial.col(0), xyTable_partial.col(1));
   const double auc_prand = trap_roc(xyTable_partial.col(0), xyTable_partial.col(0));

   // Results matrix
   arma::mat result(1, 4);

   // Handle edge cases
   if(auc_pmodel == 0 || auc_prand == 0){
     result.zeros();
     return result;
   }

   // Compute ratio safely
   double auc_ratio = NA_REAL;
   if (std::abs(auc_prand) > std::numeric_limits<double>::epsilon()) {
     auc_ratio = auc_pmodel / auc_prand;
   }

   // Compute full AUC if requested
   double auc_complete = NA_REAL;
   if (compute_full_auc) {
     arma::mat xyTable_full = arma::join_horiz(fractional_area, sensibility);
     xyTable_full = xyTable_full.rows(arma::sort_index(xyTable_full.col(0)));
     auc_complete = trap_roc(xyTable_full.col(0), xyTable_full.col(1));
   }

   result(0, 0) = auc_complete;
   result(0, 1) = auc_pmodel;
   result(0, 2) = auc_prand;
   result(0, 3) = auc_ratio;

   return result;
 }
//' Execute parallel bootstrap iterations for AUC Calculation
//'
//' @description Coordinates the parallel execution of multiple bootstrap iterations for AUC metrics computation.
//' This function serves as the parallel driver for the main AUC calculation workflow.
//'
//' @param big_classpixels Numeric matrix of bin comparison values (from \code{\link{bigclass_matrix}})
//' @param fractional_area Numeric vector of cumulative fractional areas (x-axis values)
//' @param test_prediction Numeric vector of binned test predictions
//' @param n_samp Integer specifying number of test observations to sample per iteration
//' @param error_sens Double specifying sensitivity threshold for partial AUC
//' @param n_iterations Integer specifying number of bootstrap iterations
//' @param compute_full_auc Boolean indicating whether to compute complete AUC
//'
//' @return A numeric matrix with `n_iterations` rows and 4 columns containing:
//' \itemize{
//'   \item auc_complete: Complete AUC (NA when compute_full_auc = FALSE)
//'   \item auc_pmodel: Partial AUC for the model
//'   \item auc_prand: Partial AUC for random model
//'   \item ratio: Ratio of model AUC to random AUC
//' }
//'
//' @details
//' This function manages the bootstrap process by:
//' 1. Creating a results matrix to store outputs from all iterations
//' 2. Using OpenMP to parallelize iterations across available cores
//' 3. For each iteration:
//'    - Calls \code{\link{calc_aucDF_arma}} to compute AUC metrics
//'    - Stores results in the output matrix
//'
//' @section Parallel Execution:
//' - Iterations are distributed across available CPU cores
//' - Each thread computes one bootstrap iteration independently
//' - Thread-safe through:
//'   * Private result storage per iteration
//'   * Atomic writes to results matrix
//'
//' @section Performance Notes:
//' - Scaling is approximately linear with core count
//' - Memory overhead is minimal (shared input data, private result rows)
//' - Critical for efficient bootstrap implementation
//'
arma::mat iterate_aucDF_arma_opt(
     const arma::mat& big_classpixels,
     const arma::vec& fractional_area,
     const arma::vec& test_prediction,
     int n_samp,
     double error_sens,
     int n_iterations,
     bool compute_full_auc) {

   // Create results matrix with 4 columns
   arma::mat results(n_iterations, 4);

#pragma omp parallel for
   for (int i = 0; i < n_iterations; ++i) {
     results.row(i) = calc_aucDF_arma(
       big_classpixels, fractional_area, test_prediction,
       n_samp, error_sens, compute_full_auc
     );
   }

   return results;
 }

//' Parallel AUC and partial AUC calculation with optimized memory usage
//'
//' @description Computes bootstrap estimates of partial and complete AUC using parallel processing and optimized binning.
//'
//' @param test_prediction Numeric vector of test prediction values
//' @param prediction Numeric vector of model predictions (background suitability data)
//' @param threshold Percentage threshold for partial AUC calculation (default = 5.0)
//' @param sample_percentage Percentage of test data to sample in each iteration (default = 50.0)
//' @param iterations Number of bootstrap iterations (default = 500)
//' @param compute_full_auc Boolean indicating whether to compute complete AUC (default = TRUE)
//' @param n_bins Number of bins for discretization (default = 500)
//'
//' @return A numeric matrix with `iterations` rows and 4 columns containing:
//' \itemize{
//'   \item auc_complete: Complete AUC (NA when compute_full_auc = FALSE)
//'   \item auc_pmodel: Partial AUC for the model (sensitivity > 1 - threshold/100)
//'   \item auc_prand: Partial AUC for random model (reference)
//'   \item ratio: Ratio of model AUC to random AUC (model/reference)
//' }
//'
//' @details
//' This function implements a highly optimized AUC calculation pipeline:
//' 1. Cleans input data (removes non-finite values)
//' 2. Combines background and test predictions
//' 3. Performs range-based binning (discretization)
//' 4. Computes cumulative distribution of background predictions
//' 5. Runs bootstrap iterations in parallel:
//'    - Samples test predictions
//'    - Computes sensitivity-specificity curves
//'    - Calculates partial and complete AUC
//'
//' Key optimizations:
//' - OpenMP parallelization for binning and bootstrap
//' - Vectorized operations using Armadillo
//'
//' @section Partial AUC:
//' The partial AUC focuses on the high-sensitivity region defined by:
//' Sensitivity > 1 - (threshold/100)
//'
//' @examples
//' # Basic usage with random data
//' set.seed(123)
//' bg_pred <- runif(1000)   # bg predictions
//' test_pred <- runif(500)     # Test predictions
//'
//' # Compute only partial AUC metrics (500 iterations)
//' results <- auc_parallel(test_pred, bg_pred,
//'                             threshold = 5.0,
//'                             iterations = 100)  # Reduced for example
//'
//' # View first 5 iterations
//' head(results, 5)
//'
//' # Summarize results (assume complete AUC was not computed)
//' summary <- summarize_auc_results(results, has_complete_auc = FALSE)
//'
//' # Interpretation:
//' # - auc_pmodel: Model's partial AUC (higher is better)
//' # - auc_prand: Random model's partial AUC
//' # - ratio: Model AUC / Random AUC (>1 indicates better than random)
//'
//' # Compute both partial and complete AUC
//' full_results <- auc_parallel(test_pred, bg_pred,
//'                                  compute_full_auc = TRUE,
//'                                  iterations = 100)
//'
//'
//' @seealso \code{\link{summarize_auc_results}} for results processing,
//'          \code{\link{trap_roc}} for integration method
//' @export
// [[Rcpp::export]]
arma::mat auc_parallel(const arma::vec& test_prediction,
                            const arma::vec& prediction,
                            double threshold = 5.0,
                            double sample_percentage = 50.0,
                            int iterations = 500,
                            bool compute_full_auc = true,
                            int n_bins = 500) {

   // Input validation
   if (test_prediction.n_elem == 0 || prediction.n_elem == 0) {
     stop("Input vectors cannot be empty");
   }

   if (n_bins <= 1) {
     stop("Number of bins must be greater than 1");
   }

   // Process vectors
   arma::vec test_clean = test_prediction.elem(arma::find_finite(test_prediction));
   arma::vec pred_clean = prediction.elem(arma::find_finite(prediction));

   if (pred_clean.n_elem == 0 || test_clean.n_elem == 0) {
     stop("No finite values in prediction vectors");
   }

   // Combined vector
   arma::vec combined = arma::join_cols(pred_clean, test_clean);

   // Binning
   const double min_val = combined.min();
   const double max_val = combined.max();
   const double range = max_val - min_val;

   if (range <= std::numeric_limits<double>::epsilon()) {
     stop("All prediction values are identical");
   }

   const double scale = (n_bins - 1.0) / range;
   arma::vec binned = (combined - min_val) * scale;

   // Safe binning with clamping
   binned.transform([n_bins](double val) {
     val = std::floor(val);
     val = std::max(0.0, std::min(static_cast<double>(n_bins-1), val));
     return val + 1.0;
   });

   // Split binned vectors
   const int nprediction = pred_clean.n_elem;
   if (nprediction >= binned.n_elem) {
     stop("Invalid vector sizes after binning");
   }

   arma::vec test_binned = binned.subvec(nprediction, binned.n_elem - 1);
   arma::vec bg_binned = binned.subvec(0, nprediction - 1);

   // Parallel histogram counting
   arma::ivec counts(n_bins, arma::fill::zeros);
   const arma::ivec bg_binned_int = arma::conv_to<arma::ivec>::from(bg_binned) - 1;

#pragma omp parallel for
   for (uword i = 0; i < bg_binned_int.n_elem; ++i) {
     int val = bg_binned_int[i];
     if (val >= 0 && val < n_bins) {
#pragma omp atomic
       counts[n_bins - 1 - val]++;
     }
   }

   // Statistics - PROTECT AGAINST DIVISION BY ZERO
   arma::vec csum = arma::cumsum(arma::conv_to<arma::vec>::from(counts));
   arma::vec percent;

   if (csum.n_elem > 0 && csum.back() > std::numeric_limits<double>::epsilon()) {
     percent = csum / csum.back();
   } else {
     // Handle zero cumulative sum case
     percent = arma::vec(csum.n_elem, arma::fill::zeros);
   }

   // Parameters - ensure at least 1 sample
   const double error_sens = 1.0 - (threshold / 100.0);
   const int n_samp = std::max(1, static_cast<int>(
     std::ceil((sample_percentage / 100.0) * test_binned.n_elem)
   ));

   // Matrix creation with validation
   arma::mat big_classpixels(n_samp, n_bins);
   for (uword i = 0; i < n_bins; ++i) {
     big_classpixels.col(i).fill(n_bins - i);
   }

   // Parallel AUC calculation
   return iterate_aucDF_arma_opt(big_classpixels, percent, test_binned,
                                 n_samp, error_sens, iterations, compute_full_auc);
 }

//' Summarize Bootstrap AUC Results
//'
//' Computes aggregated statistics from bootstrap AUC iterations. This function processes
//' the raw output of \code{\link{auc_parallel}} to produce meaningful summary metrics of the
//' partial ROC test.
//'
//' @param auc_results Numeric matrix output from \code{\link{auc_parallel}}
//'        (dimensions: n_iterations x 4)
//' @param has_complete_auc Boolean indicating whether complete AUC was computed in the
//'        bootstrap iterations (affects first summary column)
//'
//' @return A numeric matrix with 1 row and 5 columns containing:
//' \itemize{
//'   \item mean_complete_auc: Mean of complete AUC values (NA if not computed)
//'   \item mean_pauc: Mean of partial AUC values for the model
//'   \item mean_pauc_rand: Mean of partial AUC values for random model (reference)
//'   \item mean_auc_ratio: Mean of AUC ratios (model/random)
//'   \item prop_ratio_gt1: Proportion of iterations where ratio > 1 (performance better than random)
//' }
//'
//' @details
//' This function:
//' 1. Filters iterations with non-finite ratio values (handles bootstrap failures)
//' 2. Computes means for each AUC metric across valid iterations
//' 3. Calculates proportion of iterations where model outperforms random (ratio > 1).
//'    This way of computing the the p-value of the test.
//'
//' Special handling:
//' - Returns all NAs if no valid iterations exist
//' - First column (complete AUC) depends on \code{has_complete_auc} parameter
//' - Handles NaN/Inf values safely by filtering
//'
//' @section Interpretation Guide:
//' - \code{mean_auc_ratio > 1}: Model generally outperforms random predictions
//' - \code{prop_ratio_gt1 = 1.9}: 90% of iterations showed better-than-random performance
//' - \code{mean_pauc}: Absolute performance measure (higher = better discrimination)
//'
//' @examples
//' # Basic usage with simulated results
//' set.seed(123)
//' # Simulate bootstrap output (100 iterations x 4 metrics)
//' auc_matrix <- cbind(
//'   complete = rnorm(100, 0.85, 0.05),  # Complete AUC
//'   pmodel   = rnorm(100, 0.15, 0.03),  # Partial model AUC
//'   prand    = rnorm(100, 0.08, 0.02),  # Partial random AUC
//'   ratio    = rnorm(100, 1.9, 0.4)     # Ratio
//' )
//'
//' # Summarize results (assuming complete AUC was computed)
//' summary <- summarize_auc_results(auc_matrix, has_complete_auc = TRUE)
//'
//' # Typical output interpretation:
//' # - mean_complete_auc: 0.85 (good overall discrimination)
//' # - mean_pauc: 0.15 (absolute partial AUC)
//' # - mean_pauc_rand: 0.08 (random expectation)
//' # - mean_pAUCratio: 1.9 (model 90% better than random)
//' # - p_value: 0.98 (98% of iterations showed model > random)
//'
//' # Real-world usage with actual AUC function output
//' \donttest{
//' # First run bootstrap AUC calculation
//' bg_pred <- runif(1000)
//' test_pred <- runif(500)
//' auc_output <- auc_parallel(
//'   test_prediction = test_pred,
//'   prediction = bg_pred,
//'   iterations = 100
//' )
//'
//' # Then summarize results (complete AUC not computed in this case)
//' summary <- summarize_auc_results(auc_output, has_complete_auc = FALSE)
//'
//' # Print summary statistics
//' colnames(summary) <- c("mean_complete_auc", "mean_pauc",
//'                       "mean_pauc_rand", "mean_pAUCratio", "p_value")
//' print(summary)
//'
//' # Expected output structure:
//' #      mean_complete_auc mean_pauc mean_pauc_rand mean_pAUCratio    p_value
//' # [1,]               NA     0.152          0.083       1.83           0.94
//' }
//'
//' @seealso \code{\link{auc_parallel}} for generating the input matrix
//' @export
// [[Rcpp::export]]
arma::mat summarize_auc_results(const arma::mat& auc_results, bool has_complete_auc) {

  // Extract the ratio column (column 3)
  arma::vec ratios = auc_results.col(3);
  // Find finite values
  arma::uvec finite_idx = arma::find_finite(ratios);
  // AUC
  arma::vec auc = auc_results.col(0);
  // pAUC
  arma::vec pauc = auc_results.col(1);
  // rando pauc
  arma::vec rpauc = auc_results.col(2);

  arma::vec finite_ratios = ratios(finite_idx);
  // Calculate statistics
  arma::mat summary(1, 5);

  double prp_succ = arma::accu(ratios > 1.0) / (ratios.n_elem*1.0);
  if(finite_ratios.n_elem == 0){
    summary(0, 0) = NA_REAL;
    summary(0, 1) = NA_REAL;
    summary(0, 2) = NA_REAL;
    summary(0, 3) = NA_REAL;
    summary(0, 4) = NA_REAL;
  } else{
    summary(0, 0) = has_complete_auc ? arma::mean(auc(finite_idx)) : NA_REAL;
    summary(0, 1) = arma::mean(pauc(finite_idx));
    summary(0, 2) = arma::mean(rpauc(finite_idx));
    summary(0, 3) = arma::mean(finite_ratios);
    summary(0, 4) = 1 - prp_succ;
  }



  return summary;
}
