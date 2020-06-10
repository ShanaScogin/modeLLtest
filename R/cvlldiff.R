#'This function implements the cross-validated difference in means (CVDM)
#'test between two vectors of cross-validated log-likelihoods. A positive test
#'statistic supports the method that produced the first vector and a negative
#'test statistic supports the second.
#'
#'@title Cross-Validated Difference in Means (CVDM) Test with Vector Imputs
#'@description Applies cross-validated log-likelihood to test between
#'two methods of estimating a formula. The output identifies the vector
#'from the more appropriate model.
#'
#'Please cite:
#'
#'Desmarais, B. A., & Harden, J. J. (2014). An Unbiased Model Comparison Test Using
#'Cross-Validation. Quality & Quantity, 48(4), 2155-2173.
#'\href{https://doi.org/10.1007/s11135-013-9884-7}{https://doi.org/10.1007/s11135-013-9884-7}
#'@param vector1 A numeric vector of cross-validated log-likelihoods.
#'@param vector2 A numeric vector of cross-validated log-likelihoods.
#'@param df A value of the degrees of freedom in the models.
#'@return An object of class \code{cvlldiff} computed by the cross-validated log likelihood
#'difference in means test (CVDM). The test statistic object is the Cross-Validated
#'Johnson's t-test. A positive test statistic supports the first method and a negative test
#'statistic supports the second.See \code{\link{cvdm_object}} for more details.
#'@references
#'Desmarais, B. A., & Harden, J. J. (2014). An Unbiased Model Comparison Test Using
#'Cross-Validation. Quality & Quantity, 48(4), 2155-2173.
#'\href{https://doi.org/10.1007/s11135-013-9884-7}{https://doi.org/10.1007/s11135-013-9884-7}
#'@examples
#' \dontshow{.old_wd <- setwd(tempdir())}
#' \donttest{
#'   set.seed(123456)
#'   b0 <- .2 # True value for the intercept
#'   b1 <- .5 # True value for the slope
#'   n <- 500 # Sample size
#'   X <- runif(n, -1, 1)
#'
#'   Y <- b0 + b1 * X + rnorm(n, 0, 1) # N(0, 1 error)
#'   cvll_ols <- cvll(Y ~ X, data.frame(cbind(Y, X)), method = "OLS")
#'   cvll_mr <- cvll(Y ~ X, data.frame(cbind(Y, X)), method = "MR")
#'   obj_compare <- cvlldiff(cvll_ols$cvll, cvll_mr$cvll, cvll_ols$df)
#' }
#' \dontshow{setwd(.old_wd)}
#'@export

cvlldiff <- function(vector1,
                 vector2,
                 df){

  # Find the difference
  cvlldiff <- as.numeric(vector1) - as.numeric(vector2) # cross-validated log likelihood diff
  test_stat <- johnsons_t(cvlldiff)
  if (length(vector1) != length(vector2)){
    stop ("Vectors must be the same length")
  }

  if (missing(df)) {
    p_value <- c("Not available due to empty degrees of freedom argument")
  } else {
    p_value <- ifelse (test_stat > 0,
                       pt(test_stat, df = df, # student t distrib
                          lower.tail = FALSE),
                       pt(test_stat, df = df)) # student t distrib
  }
  # Positive test statistics support method 1
  # Negative test statistics support method 2
  best <- ifelse(test_stat > 0, "the first vector", "the second vector")
  obj <- list(best = best,
              test_stat = test_stat,
              p_value = p_value)

  class(obj) <- "cvlldiff"

  obj

}

mu3hat <- function(x){
  n <- length(x)
  ns <- n / ((n - 1) * (n - 2))
  ns * sum( (x - mean(x) ) ^ 3)
}

johnsons_t <- function(x){ # input is cross-validated log likelihood difs
  m3 <- mu3hat(x)
  s <- sd(x)
  n <- length(x)
  (mean(x) + m3 / (6 * s ^ 2 * n) + m3 / (3 * s ^ 4) * mean(x) ^ 2) * sqrt(n) / s
}
