#' Summarize Simulation Results
#' 
#' Creates table summarizing results of statistical simulations, providing 
#' common metrics of performance like mean bias, standard deviation, mean 
#' standard error, mean squared error, and confidence interval coverage.
#' 
#' @param estimates Numeric matrix where each column gives the point estimates 
#' for a particular method across multiple trials.
#' @param ses Numeric matrix where each column gives the standard errors for 
#' a particular method across multiple trials.
#' @param truth Numeric value specifying the true value of the parameter 
#' being estimated.
#' @param statistics Numeric vector specifying which performance metrics should 
#' be calculated. Possible values are \code{"mean"}, \code{"median"}, 
#' \code{"mean_bias"}, \code{"median_bias"}, \code{"sd"}, \code{"iqr"}, 
#' \code{"mean_se"} (for mean standard error), \code{"mse"} (for mean squared 
#' error), and \code{"coverage"} (for confidence interval coverage).
#' @param alpha Numeric value specifying alpha for confidence interval. Set to 
#' \code{0.05} for the usual 95\% CI, \code{0.1} for a 90\% CI, and so forth.
#' @param digits Numeric value or vector specifying the number of decimal places 
#' to include.
#' 
#' @return Numeric matrix.
#' 
#' @examples
#' # For X ~ N(mu, sigma^2), the MLE for sigma^2 is the sample variance with n 
#' # in the denominator, but the unbiased version with (n - 1) is typically used 
#' # for its unbiasedness. Compare these estimators in 1,000 trials with n = 25. 
#' MLE <- c()
#' Unbiased <- c()
#' for (ii in 1: 1000) {
#'    x <- rnorm(n = 25)
#'    MLE[ii] <- sum((x - mean(x))^2) / 25
#'    Unbiased[ii] <- sum((x - mean(x))^2) / 24
#'  }
#' sumsim(estimates = cbind(MLE, Unbiased), truth = 1)
#' 
#' @export
sumsim <- function(estimates, ses = NULL,
                   truth = NULL,
                   statistics = c("mean_bias", "sd", "mean_se", "mse",
                                  "coverage"),
                   alpha = 0.05,
                   digits = 3) {
  
  # Remove statistics that can't be calculated from specified inputs
  if (is.null(truth)) {
    statistics <- 
      statistics[-which(statistics %in% 
                          c("mean_bias", "median_bias", "mse", "coverage"))]
  }
  if (is.null(ses)) {
    statistics <- statistics[-which(statistics %in% c("mean_se", "coverage"))]
  }
  
  # Create values for sprintf
  if (length(digits) == 1) {
    digits <- rep(digits, length(statistics))
  }
  # sprs <- paste("%.", digits, "f", sep = "")
  
  # Initialize matrix
  mat <- matrix(NA, ncol = length(statistics), nrow = ncol(estimates))
  mat.colnames <- c()
  mat.rownames <- colnames(estimates)
  if (is.null(mat.rownames)) {
    mat.rownames <- paste("Method", 1: ncol(estimates))
  }
  
  # If CI coverage requested, get z value
  if ("coverage" %in% statistics) {
    zval <- qnorm(1 - (alpha / 2))
  }
  
  # Calculate requested statistics
  index <- 0
  for (ii in 1: length(statistics)) {
    index <- index + 1
    statistic.ii <- statistics[ii]
    if (statistic.ii == "mean") {
      mat[, index] <- round(apply(estimates, 2, mean), digits[ii])
      mat.colnames[ii] <- "Mean"
    } else if (statistic.ii == "median") {
      mat[, index] <- round(apply(estimates, 2, median), digits[ii])
      mat.colnames[ii] <- "Median"
    } else if (statistic.ii == "mean_bias") {
      mat[, index] <- round(apply(estimates, 2, mean) - truth, digits[ii])
      mat.colnames[ii] <- "Mean Bias"
    } else if (statistic.ii == "median_bias") {
      mat[, index] <- round(apply(estimates, 2, median) - truth, digits[ii])
      mat.colnames[ii] <- "Median Bias"
    } else if (statistic.ii == "sd") {
      mat[, index] <- round(apply(estimates, 2, sd), digits[ii])
      mat.colnames[ii] <- "SD"
    } else if (statistic.ii == "iqr") {
      mat[, index] <- round(apply(estimates, 2, IQR), digits[ii])
      mat.colnames[ii] <- "IQR"
    } else if (statistic.ii == "mean_se") {
      mat[, index] <- round(apply(ses, 2, mean), digits[ii])
      mat.colnames[ii] <- "Mean SE"
    } else if (statistic.ii == "mse") {
      mat[, index] <- round(apply(estimates, 2, function(x) mean((x - truth)^2)), 
                            digits[ii])
      mat.colnames[ii] <- "MSE"
    } else if (statistic.ii == "coverage") {
      for (jj in 1: ncol(estimates)) {
        mat[jj, index] <-
          round(mean(inside(x = truth, 
                            ends = cbind(estimates[, jj] - zval * ses[, jj], 
                                         estimates[, jj] + zval * ses[, jj]))), 
                digits[ii])
      }
      mat.colnames[ii] <- "Coverage"
    }
  }
  
  # Return mat with appropriate row and column names
  rownames(mat) <- mat.rownames
  colnames(mat) <- mat.colnames
  return(mat)
  
}