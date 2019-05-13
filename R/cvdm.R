#'This function implements the cross-validated difference in means (CVDM)
#'test between two methods of estimating a formula.
#'
#'@title Cross-Validated Difference in Means (CVDM) Test
#'@description Applies cross-validated log-likelihood difference in means test to
#'compare two methods of estimating a formula. The output identifies the more
#'appropriate model.
#'
#'In choosing between OLS and MR, please cite:
#'
#'Harden, J. J., & Desmarais, B. A. (2011). Linear Models with Outliers:
#'Choosing between Conditional-Mean and Conditional-Median Methods.
#'State Politics & Policy Quarterly, 11(4), 371-389.
#'
#'For other applications of the CVDM test, please cite:
#'
#'Desmarais, B. A., & Harden, J. J. (2014). An Unbiased Model Comparison Test Using
#'Cross-Validation. Quality & Quantity, 48(4), 2155-2173.
#'@param formula A formula object, with the dependent variable on the
#'left of a ~ operator, and the independent variables on the right.
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model.
#'@param method1 A method to estimate the model. Currently takes
#'Ordinary Least Squares ("OLS"), Median Regression ("MR"), Robust Linear
#'Regression ("RLM") using M-estimation, and Robust Linear Regression using
#'MM-estimation ("RLM-MM"). The algorithm method used to compute the fit for the
#'median regression is the modified version of the Barrodale and Roberts algorithm
#'for l1-regression, which is the default by R package quantreg. See quantreg
#'qr function documentation for more deatils. Fitting for the robust regressions
#'is done by iterated re-weighted least squares (IWLS) and is taken from the
#'MASS package rlm function. The MM-estimation is the M-estimation with Tukey's
#'biweight initialized by a specific S-estimate. See MASS package rlm documentation
#'for details.
#'@param method2 A method to estimate the model. Options
#'are same as for method1.
#'@param subset Expression indicating which subset of the rows of data should be
#'used in the fit. All observations are included by default.
#'@param na.action A missing-data filter function, applied to the model.frame,
#'after any subset argument has been used.
#'@return An object of class \code{cvdm} computed by the cross-validated log likelihood
#'difference in means test (CVDM). The object is the Cross-Validated Johnson's t-test.
#'A positive test statistic supports the first method and a negative test statistic supports
#'the second. See \code{cvdm_object} for more details.
#'@examples
#' \dontrun{
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
#'@export

cvdm <- function(formula,
                 data,
                 method1 = c("OLS", "MR", "RLM"),
                 method2 = c("OLS", "MR", "RLM"),
                 subset,
                 na.action){

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

  y <- model.response(mf, "any") # e.g. factors are allowed
  x <- model.matrix(attr(mf, "terms"), data = mf)
  n_row <- length(y)
  n_col <- ncol(x)

  # Call the CVLL with first method
  if (method1 == "OLS"){
    cvll_1 <- cvll_ols(as.matrix(x), as.matrix(y), n_row,  n_col)
    m1 <- "OLS"
  } else if (method1 == "MR"){
    cvll_1 <- cvll_mr(as.matrix(x), y, n_row)
    m1 <- "MR"
  } else if (method1 == "RLM"){
    cvll_1 <- cvll_rlm_m(as.matrix(x), y, n_row, n_col)
    m1 <- "RLM"
  } else {
    print("First method unknown")
  }

  # Call the CVLL with second method
  if (method2 == "OLS"){
    cvll_2 <- cvll_ols(as.matrix(x), as.matrix(y), n_row,  n_col)
    m2 <- "OLS"
  } else if (method2 == "MR"){
    cvll_2 <- cvll_mr(as.matrix(x), y, n_row)
    m2 <- "MR"
  } else if (method2 == "RLM"){
    cvll_2 <- cvll_rlm_m(as.matrix(x), y, n_row, n_col)
    m2 <- "RLM"
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
              y = y,
              model_matrix = x)

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
