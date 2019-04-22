#'This function implements the cross-validated median fit (CVMF) test.
#'The function cvmf() tests between the partial
#'likelihood maximization (PLM) and the iteratively reweighted robust
#'(IRR) method of estimation for a given application of the Cox model.
#'The Cox model is a partial parametric model that does not make assumptions
#'about the baseline hazard. It can be estimated via PLM, the standard
#'estimator, or IRR, a robust estimator that identifies and downweights
#'outliers. The choice between the two methods involves a trade-off
#'between bias and efficiency. PLM is more efficient, but biased under
#'specification problems. IRR reduces bias, but results in high
#'variance due to the loss of efficiency. The cvmf() function returns an
#'object to identify the prefered estimation method.
#'
#'See also \code{\link{coxph}}, \code{\link{coxr}}, \code{\link{Surv}}
#'
#'@title Cross-Validated Median Fit (CVMF) Test
#'@description Applies cross-validated log-likelihood to test between
#'partial likelihood maximization (PLM) and the iteratively reweighted
#'robust (IRR) method of estimation for a given application of the Cox model.
#'For more, see: Desmarais, B. A., & Harden, J. J. (2012). Comparing partial
#'likelihood and robust estimation methods for the Cox regression model.
#'Political Analysis, 20(1), 113-135.
#'@param formula A formula object, with the response on the left of a ~
#'operator, and the terms on the right. The response must be a survival
#'object as returned by the Surv() function from the survival package.
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model
#'or in the subset and the weights argument.
#'@param method A character string specifying the method for tie handling in coxph().
#'If there are no tied death times all the methods are equivalent.
#'Following the coxph() function in the survival package, the Efron
#'approximation is used as the default. The survival package justifies this
#'due to the Efron method being is more accurate when dealing with tied death
#'times, and is as efficient computationally than the common Breslow method.
#'The "exact partial likelihood" is equivalent to a 'conditional logistic model,
#'and is appropriate when the times are a small set of discrete values.
#'This argument does not exist in the coxr() function in the coxrobust package.
#'For coxr(), method is based on a smooth modification of the partial likelihood.
#'See documentation from survival package for more on coxph() method and
#'coxrobust package for coxr() method.
#'@param trunc A value that determines the trimming level for the robust
#'estimator. The default is 0.95. Roughtly, quantile of the sample
#'\eqn{T_i exp(\beta'Z_i)}. It is an argument in the coxr() function
#'in the coxrobust package.
#'@param na.action A missing-data filter function, applied to the model.frame,
#'after any subset argument has been used.
#'@param f.weight A type of weighting function for coxr() in the coxrobust package.
#'The default is \code{quadratic}. See coxr() documentation for more.
#'@param weights A vector of case weights for coxph() in the survival package.
#'See coxph() documentation for more.
#'@param singular.ok Logical value indicating how to handle collinearity in the
#'model matrix. If \code{TRUE}, the program will automatically skip over columns
#'of the X matrix that are linear combinations of earlier columns. In this case
#'the coefficients for such columns will be NA, and the variance matrix will contain
#'zeros. For ancillary calculations, such as the linear predictor, the missing
#'coefficients are treated as zeros.
#'@param subset Expression indicating which subset of the rows of data should be
#'used in the fit. All observations are included by default.
#'@return An object of class \code{cvmf} computed by the cross-validated median fit test
#' (CVMF) to test between the PLM and IRR methods of estimating the Cox model.
#' See \code{cvmf.object} for more details.
#' @export

cvmf <- function(formula, data,
                 method = c("exact","approximate", "efron", "breslow"),
                 trunc = 0.95,
                 subset,
                 na.action,
                 f.weight = c("linear", "quadratic", "exponential"),
                 weights,
                 singular.ok = TRUE){

  call <- match.call()

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

  y <- model.extract(mf, "response")
  if ( !inherits(y, "Surv") ) {
    stop("response must be a \"Surv\" object")
  } else {
    type <- attr(y, "type")
    if ( type != "right" ) {
      stop(sprintf("\"%s\" type of survival data is not supported", type))
    }
  }

  x <- model.matrix(mterms, mf)
  x <- x[, -1, drop = FALSE]

  # trunc argument default
  if (missing(trunc)) {
    trunc <- 0.95
  }

  # f.weight argument default
  if (missing(f.weight)) {
    f.weight <- "quadratic"
  } else {
    f.weight <- f.weight
  }

  # f.weight argument default
  if (missing(method)) {
    method <- "efron"
  } else {
    method <- method
  }

  # weights argument default
  ## taken from coxph.fit.R() in survival package
  ny <- nrow(y)
  if (missing(weights)) {
    weights <- rep(1, ny)
  } else {
    weights <- weights
  }

  # na.action argument default
  ######### this really needs to be tested
  ######### might could take this out bc it's in match.call() above
  if(missing(na.action)) {
    na.action <- NULL
  } else {
    na.action <- na.action
    # coxph and coxr
  }

  # Estimate IRR
  # creating to return for comparison
  irr <- coxrobust::coxr(formula = y ~ x,
                         na.action = na.action, ### need to test this
                         trunc = trunc,
                         f.weight = f.weight, ### tested one round of this but more needed
                         singular.ok = singular.ok)

  # Estimate PLM
  # creating to return for comparison
  plm <- survival::coxph(formula = y ~ x,
                         method = method,
                         weights = weights, ##### need to test
                         na.action = na.action, ##### need to test
                         #excluding init since coxr only allows defaut
                         #excluding control
                         singular.ok = singular.ok)

  # Making empty vectors to prep for cross-validation
  n <- nrow(x)
  cvll_r <- numeric(n)
  cvll_c <- numeric(n)

  # Loop through for cross-validation
  for (i in 1:n){

    # Remove current observation
    yi <- y[-i, ]
    xi <- as.matrix(x[-i, ])
    weightsi <- weights[-i]

    # Estimate models without current observation i
    pesti <- survival::coxph(yi ~ xi,
                             method = method,
                             weights = weightsi, ##### need to test
                             na.action = na.action, ##### need to test
                             #excluding init since coxr only allows defaut
                             #excluding control
                             singular.ok = singular.ok)
    esti <- coxrobust::coxr(yi ~ xi,
                            na.action = na.action, ### need to test this
                            trunc = trunc,
                            f.weight = f.weight, ### tested one round of this but more needed
                            singular.ok = singular.ok)

    # Check if any parameters were undefined without observation i
    # This can happen with very sparse and/or very many covariates
    # (e.g., fixed effect dummies)
    na_ind <- which(is.na(pesti$coefficients))

    # Extract coefficients and covariates
    coef_p <- pesti$coefficients
    x_p <- x
    xi_p <- xi

    # Remove any covariates with undefined effects
    if (length(na_ind) > 0){
      coef_p <- pesti$coefficients[-na_ind]
      x_p <- x[, -na_ind]
      xi_p <- xi[, -na_ind]
    }

    # Compute the full and restricted partial likelihoods
    full_ll_r <- survival::coxph(y ~ offset(as.matrix(x) %*% cbind(esti$coefficients)),
                                 method = method)$loglik # this is unrestricted - fix this _r and _c
    full_ll_c <- survival::coxph(y ~ offset(as.matrix(x_p) %*% cbind(coef_p)),
                                 method = method)$loglik # this is unrestricted with nas dropped - fix this _r and _c
    esti_ll_r <- survival::coxph(yi ~ offset(as.matrix(xi) %*% cbind(esti$coefficients)),
                                 method = method)$loglik
    esti_ll_c <- survival::coxph(yi ~ offset(as.matrix(xi_p) %*% cbind(coef_p)),
                                 method = method)$loglik
    ### getting the likelihood - so just doesn't have to be irr
    ### offset() is forcing it to beta - linear predictor

    # Store
    cvll_r[i] <- full_ll_r - esti_ll_r ## why are we leaving out observations???
    cvll_c[i] <- full_ll_c - esti_ll_c
  }

  # Compute the test
  cvmf <- binom.test(sum(cvll_r > cvll_c), n, alternative = "two.sided")
  best <- ifelse(cvmf$statistic > n / 2, "IRR", "PLM")
      ## binomial test - null is just fair coin, n/2
  p <- round(cvmf$p.value, digits = 3)
  coef <- dimnames(x)[[2]]

  # Construct the returned object
  obj <- list(best = best,
              p_value = p,
              cvmf = cvmf,
              cvmf_stat = cvmf[1],
              cvmf_obs = cvmf[2],
              cvmf_p = cvmf[3],
              cvmf_ci = cvmf[4],
              coef = coef,
              irr = irr,
              plm = plm,
              irr_coefs = irr[1],
              plm_coefs = plm[1],
              irr_var = irr[5],
              plm_var = plm[2],
              irr_wald = irr[8],
              plm_wald = plm[14],
              cvpl_irr = cvll_r,
              cvpl_plm = cvll_c,
              x = x,
              y = y,
              call = call)

  class(obj) <- "cvmf"

  obj

}
