#'This function implements the cross-validated log likelihood (CVLL) test
#'between two methods of estimating a formula.
#'
#'@title Cross-Validated Log Likelihood (CVLL) Test
#'@description Applies cross-validated log-likelihood to test between
#'two methods of estimating a formula.
#'@param formula A formula object, with the dependent variable on the
#'left of a ~ operator, and the independent variables on the right.
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model.
#'@param model The name of a statistical model to estimate.
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
#'@param ... additional arguments passed to \code{zelig},
#'relevant for the model to be estimated.
#'@return An object of class \code{cvmf} computed by the cross-validated log likelihood
#'test (CVLL). See \code{cvll.object} for more details.

cvll <- function(formula, data,
                 model = c("OLS", "MR", "Poisson", "Bernoulli", "Logit", "Probit"),
                 subset,
                 na.action,
                 singular.ok = TRUE, ## right now this isn't being used
                 ...){

  call <- match.call()

  ## Model
  if(is.character(model))
    model <- get(model, mode = "function", envir = parent.frame())
  if(is.function(model)) model <- model()
  if(is.null(model$model)) {
    print(model)
    stop("'Model' not recognized")
  }

  # Extract
  mf <- match.call(expand.dots = FALSE)
  m  <- match(c("formula", "data", "subset", "na.action"), names(mf),
              nomatch = 0)
  if (m[1]==0) {
    stop("A formula argument is required")
  }
  mf <- mf[c(1, m)]
  mf[[1]] <- as.name("model.frame")
  mf <- eval(mf, parent.frame())
  if (nrow(mf) == 0) {
    stop("no (non-missing) observations")
  }

  mterms <- attr(mf, "terms")

  #### perhaps add an error message for subset?

  y <- model.response(mf, "any") # e.g. factors are allowed

  ##### look at glm() and look into null model support, check weights and offset, etc

  x <- model.matrix(mterms, mf)
  x <- x[, -1, drop = FALSE]

  # Get model functions:
  cvll <- model(x, y)
  cvll <- model$cvll

  obj <- list(cvll = cvll,
              x = x,
              y = y)

  #class(obj) <- "cvll"

  obj

}
