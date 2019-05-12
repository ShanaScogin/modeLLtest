#'This function extracts a vector of leave-one-out cross-validated
#'log likelihoods (CVLLs) from a method of estimating a formula.
#'
#'@title Cross-Validated Log Likelihood (CVLL)
#'@description Extracts the leave-one-out cross-validated log-likelihoods
#'from a method of estimating a formula.
#'@param formula A formula object, with the dependent variable on the
#'left of a ~ operator, and the independent variables on the right.
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model.
#'@param model The name of a statistical model to estimate.
#'@param method The name of a method to estimate the model. Currently takes
#'Ordinary Least Squares ("OLS"), Median Regression ("MR"), Robust Linear
#'Regression ("RLM") using M-estimation, Robust Linear Regression using
#'MM-estimation ("RLM-MM"), partial likelihood maximization (PLM), and the
#'iteratively reweighted robust (IRR) method of estimation. The algorithm method
#'used to compute the fit for the median regression is the modified version of
#'the Barrodale and Roberts algorithm for l1-regression, which is the default
#'by R package quantreg. See quantreg qr function documentation for more deatils.
#'Fitting for the robust regressions is done by iterated re-weighted least squares
#'(IWLS) and is taken from the MASS package rlm function. The MM-estimation is
#'the M-estimation with Tukey's biweight initialized by a specific S-estimate.
#'See MASS package rlm documentation for details.
#'@param subset Expression indicating which subset of the rows of data should be
#'used in the fit. All observations are included by default.
#'@param na.action A missing-data filter function, applied to the model.frame,
#'after any subset argument has been used.
#'@param singular.ok Logical value indicating how to handle collinearity in the
#'model matrix. If \code{TRUE}, the program will automatically skip over columns
#'of the X matrix that are linear combinations of earlier columns. In this case
#'the coefficients for such columns will be NA, and the variance matrix will contain
#'zeros. For ancillary calculations, such as the linear predictor, the missing
#'coefficients are treated as zeros.
#'@return An object of class \code{cvll} computed by the cross-validated log likelihood
#'(CVLL).
#' @export

### need to add more about irr and plm up in the param for method

cvll <- function(formula,
                 data,
                 method = c("OLS", "MR", "RLM"), # need to add other rlm methods and plm and irr
                 subset,
                 na.action,
                 singular.ok = TRUE){ ## right now this isn't being used

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

  #### perhaps add an error message for subset?

  y <- model.response(mf, "any") # e.g. factors are allowed
  x <- model.matrix(attr(mf, "terms"), data = mf)
  n_row <- length(y)
  n_col <- ncol(x)

  ##### look at glm() and look into null model support, check weights and offset, etc

  #  x <- model.matrix(mterms, mf)
  #  x <- x[, -1, drop = FALSE]

  # Call the CVLL
  if (method == "OLS"){
    cvll <- cvll_ols(as.matrix(x), as.matrix(y), n_row,  n_col)
    m1 <- "OLS"
  } else if (method == "MR"){
    cvll <- cvll_mr(as.matrix(x), y, n_row)
    m1 <- "MR"
  } else if (method == "RLM"){
    cvll <- cvll_rlm_m(as.matrix(x), y, n_row, n_col)
    m1 <- "RLM"
  } else {
    print("First method unknown")
  }

  df <- length(y) - ncol(x) ### fix this

  obj <- list(cvll = as.numeric(cvll),
              n = length(y),
              call = call,
              df = df,
              model_matrix = x)

  class(obj) <- "cvll"

  obj
}
