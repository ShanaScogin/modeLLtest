#'This function extracts a vector of leave-one-out cross-validated
#'log likelihoods (CVLLs) from a method of estimating a formula.
#'Singular matrices during the leave-one-out cross-validation process
#'are skipped.
#'
#'@title Cross-Validated Log Likelihood (CVLL)
#'@description Extracts the leave-one-out cross-validated log-likelihoods
#'from a method of estimating a formula.
#'@param formula A formula object, with the dependent variable on the
#'left of a ~ operator, and the independent variables on the right.
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model.
#'@param method A method to estimate the model. Currently takes
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
#'@param subset Expression indicating which subset of the rows of data should be
#'used in the fit. All observations are included by default.
#'@param na.action A missing-data filter function, applied to the model.frame,
#'after any subset argument has been used.
#'@param ... Optional arguments, currently unsupported.
#'@return An object of class \code{cvll} computed by the cross-validated log likelihood
#'(CVLL). See \code{\link{cvdm_object}} for more details.
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
#'   obj_cvll <- cvll(Y ~ X, data.frame(cbind(Y, X)), method = "OLS")
#' }
#' \dontshow{setwd(.old_wd)}
#'@export

cvll <- function(formula,
                 data,
                 method = c("OLS", "MR", "RLM", "RLM-MM"),
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

  # Call the CVLL
  if (method == "OLS"){
    cvll <- cvll_ols(as.matrix(x), y, n_row,  n_col)
    m1 <- "OLS"
  } else if (method == "MR"){
    cvll <- cvll_mr(as.matrix(x), y, n_row, n_col)
    m1 <- "MR"
  } else if (method == "RLM"){
    cvll <- cvll_rlm_m(as.matrix(x), y, n_row, n_col)
    m1 <- "RLM"
  } else if (method == "RLM-MM"){
    cvll <- cvll_rlm_mm(as.matrix(x), y, n_row, n_col)
    m2 <- "RLM-MM"
  } else {
    print("Method unknown")
  }

  df <- length(y) - ncol(x)

  obj <- list(cvll = as.numeric(cvll),
              n = length(y),
              df = df,
              method = method,
              call = call,
              x = x,
              y = y)

  class(obj) <- "cvll"

  obj
}
