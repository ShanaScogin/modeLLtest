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
#'@return An object of class \code{cvll} computed by the cross-validated log likelihood
#'test (CVLL). See \code{cvll.object} for more details.
#' dontrun{
#'
#' cvll <- function(formula,
#'                  #formulaB, ## this can compare model fit with another form
#'                  data,
#'                  method = c("OLS", "MR", "Poisson", "Bernoulli", "Logit", "Probit"),
#'                  subset,
#'                  na.action,
#'                  singular.ok = TRUE, ## right now this isn't being used
#'                  ...){
#'
#'   call <- match.call()
#'
#'   # Extract
#'   mf <- match.call(expand.dots = FALSE)
#'   m  <- match(c("formula", "data", "subset", "na.action"), names(mf),
#'               nomatch = 0)
#'   if (m[1]==0) {
#'     stop("A formula argument is required")
#'   }
#'   mf <- mf[c(1, m)]
#'   mf[[1]] <- as.name("model.frame")
#'   mf <- eval(mf, parent.frame())
#'   if (nrow(mf) == 0) {
#'     stop("no (non-missing) observations")
#'   }
#'   mterms <- attr(mf, "terms")
#'
#'   #### perhaps add an error message for subset?
#'
#'   y <- model.response(mf, "any") # e.g. factors are allowed
#'
#'   ##### look at glm() and look into null model support, check weights and offset, etc
#'
#'   x <- model.matrix(mterms, mf)
#'   x <- x[, -1, drop = FALSE]
#'
#'   # Call the CVLLs
#'   if(model == "OLS"){
#'     cvll <- cvll_ols(x, y)
#'   }
#'
#'   # Find the difference
#'   cvlldiff <- cvll[[1]] - cvll[[2]] # cross-validated log likelihood difference
#'   test_stat <- johnsons_t(cvlldiff)
#'   p_value <- ifelse (test_stat > 0,
#'                      pt(test_stat, df = cvll[[5]], # student t distrib
#'                         lower.tail = FALSE),
#'                      pt(test_stat, df = cvll[[5]])) # student t distrib
#'   # Positive test statistics support model 1
#'   # Negative test statistics support model 2
#'   best <- ifelse(test_stat > 0, "Model 1", "Model 2")
#'   obj <- list(best = best,
#'               test_stat = test_stat,
#'               p_value = p_value,
#'               n = cvll[3],
#'               model_1_stat = cvll[1],
#'               model_2_stat = cvll[2],
#'               missing_obs = cvll[4],
#'               df = cvll[5],
#'               call = call,
#'               model_matrix = x)
#'
#'   class(obj) <- "cvll"
#'
#'   obj
#'
#' }
#'
#' mu3hat <- function(x){
#'   n <- length(x)
#'   ns <- n / ((n - 1) * (n - 2)) # change object name?
#'   ns * sum( (x - mean(x) ) ^ 3)
#' }
#'
#' johnsons_t <- function(x){ # input is cross-validated log likelihood difs
#'   m3 <- mu3hat(x)
#'   s <- sd(x)
#'   n <- length(x)
#'   (mean(x) + m3 / (6 * s ^ 2 * n) + m3 / (3 * s ^ 4) * mean(x) ^ 2) * sqrt(n) / s
#' }
