#'This function implements the cross-validated difference in means (CVDM)
#'test between two methods of estimating a formula. The function takes
#'a formula and two methods and computes a vector of cross-validated log-
#'likelihoods (CVLLs) for each method using the leave-one-out method. These
#'output test score is the cross-validated Johnson's t-test. A positive test
#'statistic supports the first method and a negative test statistic supports
#'the second. Singular matrices during the leave-one-out cross-validation process
#'are skipped.
#'
#'@title Cross-Validated Difference in Means (CVDM) Test
#'@description Applies cross-validated log-likelihood difference in means test to
#'compare two methods of estimating a formula. The output identifies the more
#'appropriate model.
#'
#'In choosing between OLS and MR, please cite:
#'
#'\itemize{
#'\item Harden, J. J., & Desmarais, B. A. (2011). Linear Models with Outliers:
#'Choosing between Conditional-Mean and Conditional-Median Methods.
#'State Politics & Policy Quarterly, 11(4), 371-389.
#'\href{https://doi.org/10.1177/1532440011408929}{https://doi.org/10.1177/1532440011408929}
#'}
#'
#'For other applications of the CVDM test, please cite:
#'
#'\itemize{
#'\item Desmarais, B. A., & Harden, J. J. (2014). An Unbiased Model Comparison Test Using
#'Cross-Validation. Quality & Quantity, 48(4), 2155-2173.
#'\href{https://doi.org/10.1007/s11135-013-9884-7}{https://doi.org/10.1007/s11135-013-9884-7}
#'}
#'@param formula A formula object, with the dependent variable on the
#'left of a ~ operator, and the independent variables on the right.
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model.
#'@param method1 A method to estimate the model. Currently takes
#'Ordinary Least Squares ("OLS"), Median Regression ("MR"), Robust Linear
#'Regression ("RLM") using M-estimation, and Robust Linear Regression using
#'MM-estimation ("RLM-MM"). The algorithm method used to compute the fit for the
#'median regression is the modified version of the Barrodale and Roberts algorithm
#'for l1-regression, which is the \code{\link[quantreg]{rq}} default by R package quantreg.
#'See quantreg \code{\link[quantreg]{rq}} function documentation for more details.
#'Fitting for the robust regressions is done by iterated re-weighted least squares
#'(IWLS) and is taken from the MASS package \code{\link[MASS]{rlm}} function.
#'The MM-estimation is the M-estimation with Tukey's biweight initialized by a specific
#'S-estimate. The M-estimation, which can be achieved in this package with the
#'option "RLM", is the default for the MASS \code{\link[MASS]{rlm}}
#'function. See MASS package \code{\link[MASS]{rlm}} documentation for details.
#'@param method2 A method to estimate the model. Options
#'are same as for method1.
#'@param subset Expression indicating which subset of the rows of data should be
#'used in the fit. All observations are included by default.
#'@param na.action A missing-data filter function, applied to the model.frame,
#'after any subset argument has been used.
#'@param ... Optional arguments, currently unsupported.
#'@return An object of class \code{cvdm} computed by the cross-validated log likelihood
#'difference in means test (CVDM). The object is the Cross-Validated Johnson's t-test.
#'A positive test statistic supports the first method and a negative test statistic supports
#'the second. See \code{\link{cvdm_object}} for more details.
#'@references \itemize{
#'\item Harden, J. J., & Desmarais, B. A. (2011). Linear Models with Outliers:
#'Choosing between Conditional-Mean and Conditional-Median Methods.
#'State Politics & Policy Quarterly, 11(4), 371-389.
#'\href{https://doi.org/10.1177/1532440011408929}{https://doi.org/10.1177/1532440011408929}
#'
#'\item Desmarais, B. A., & Harden, J. J. (2014). An Unbiased Model Comparison Test Using
#'Cross-Validation. Quality & Quantity, 48(4), 2155-2173.
#'\href{https://doi.org/10.1007/s11135-013-9884-7}{https://doi.org/10.1007/s11135-013-9884-7}
#'}
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
#'
#'   obj_cvdm <- cvdm(Y ~ X, data.frame(cbind(Y, X)), method1 = "OLS", method2 = "MR")
#' }
#' \dontshow{setwd(.old_wd)}
#'@export

cvdm <- function(formula,
                 data,
                 method1 = c("OLS", "MR", "RLM", "RLM-MM"),
                 method2 = c("OLS", "MR", "RLM", "RLM-MM"),
                 subset,
                 na.action,
                 ...){

  call <- match.call()

  # Extract
  mf <- match.call(expand.dots = FALSE)
  m  <- match(c("formula", "data", "subset", "na.action"), names(mf),
              nomatch = 0)
  if (m[1]==0) {
    stop("Please enter a formula")
  }
  mf <- mf[c(1, m)]
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  if (nrow(mf) == 0) {
    print("no (non-missing) observations")
  }
  mterms <- attr(mf, "terms")

  y <- model.response(mf, "any")
  x <- model.matrix(attr(mf, "terms"), data = mf)
  n_row <- length(y)
  n_col <- ncol(x)

  # Call the CVLL with first method
  if (method1 == "OLS"){
    cvll_1 <- cvll_ols(as.matrix(x), y, n_row,  n_col)
    m1 <- "OLS"
  } else if (method1 == "MR"){
    cvll_1 <- cvll_mr(as.matrix(x), y, n_row, n_col)
    m1 <- "MR"
  } else if (method1 == "RLM"){
    cvll_1 <- cvll_rlm_m(as.matrix(x), y, n_row, n_col)
    m1 <- "RLM"
  } else if (method1 == "RLM-MM"){
    cvll_1 <- cvll_rlm_mm(as.matrix(x), y, n_row, n_col)
    m1 <- "RLM-MM"
  } else {
    print("First method unknown")
  }

  # Call the CVLL with second method
  if (method2 == "OLS"){
    cvll_2 <- cvll_ols(as.matrix(x), y, n_row,  n_col)
    m2 <- "OLS"
  } else if (method2 == "MR"){
    cvll_2 <- cvll_mr(as.matrix(x), y, n_row, n_col)
    m2 <- "MR"
  } else if (method2 == "RLM"){
    cvll_2 <- cvll_rlm_m(as.matrix(x), y, n_row, n_col)
    m2 <- "RLM"
  } else if (method2 == "RLM-MM"){
    cvll_2 <- cvll_rlm_mm(as.matrix(x), y, n_row, n_col)
    m2 <- "RLM-MM"
  } else {
    print("Second method unknown")
  }

  # Find the difference
  df <- length(y) - ncol(x)
  cvlldiff <- as.numeric(cvll_1) - as.numeric(cvll_2) # cross-val log likelihood diff
  test_stat <- johnsons_t(cvlldiff)
  p_value <- ifelse (test_stat > 0,
                     pt(test_stat, df = df, # student t distrib
                        lower.tail = FALSE),
                     pt(test_stat, df = df)) # student t distrib
  # Positive test statistics support method 1
  # Negative test statistics support method 2
  best <- ifelse(test_stat > 0, m1, m2)
  obj <- list(best = best,
              test_stat = test_stat,
              p_value = p_value,
              n = length(y),
              df = df,
              call = call,
              x = x,
              y = y)

  class(obj) <- "cvdm"

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
