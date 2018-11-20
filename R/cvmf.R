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
#'variance due to the loss of efficiency. This function returns an
#'object to distinguish the prefered estimation.
#'
#'See also \code{\link{coxph}}, \code{\link{coxr}}, \code{\link{Surv}}
#'
#'@title Cross-Validated Median Fit (CVMF) Test
#'@description Applies cross-validated log-likelihood to test between
#'partial likelihood maximization (PLM) and the iteratively reweighted
#'robust (IRR) method of estimation for a given application of the Cox model.
#'@param formula A formula object, with the response on the left of a ~
#'operator, and the terms on the right. The response must be a survival
#'object as returned by the Surv() function from the survival package.
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model
#'or in the subset and the weights argument.
#'@param method A character string specifying the method for tie handling.
#'If there are no tied death times all the methods are equivalent.
#'Following the coxph() function in the survival package, the Efron
#'approximation is used as the default. The survival package justifies this
#'due to the Efron method being is more accurate when dealing with tied death
#'times, and is as efficient computationally than the common Breslow method.
#'The "exact partial likelihood" is equivalent to a 'conditional logistic model,
#'and is appropriate when the times are a small set of discrete values.
#'See documentation from survival package for more.
#'@param trunc A value that determines the trimming level for the robust
#'estimator. The default is 0.95. Roughtly, quantile of the sample T_i exp(β'Z_i).
#'It is an argument in the coxr() function in the coxrobust package.
#'@param na.action A missing-data filter function, applied to the model.frame,
#'after any subset argument has been used.
#'@param f.weight
#'@param weight
#'@param init
#'@param control
#'@param singular.ok Logical value indicating how to handle collinearity in the
#'model matrix. If \code{TRUE}, the program will automatically skip over columns
#'of the X matrix that are linear combinations of earlier columns. In this case
#'the coefficients for such columns will be NA, and the variance matrix will contain
#'zeros. For ancillary calculations, such as the linear predictor, the missing
#'coefficients are treated as zeros.
#'@param model
#'@param subset Expression indicating which subset of the rows of data should be
#'used in the fit. All observations are included by default.
#'@return An object computed by the cross-validated median fit test
#' (CVMF) to test between the PLM and IRR methods of estimating the Cox model.

cvmf <- function(formula, data, method, trunc, na.action,
                 f.weight, weights, init, control, singular.ok,
                 model, subset){

  # method argument default
  if(missing(method)) {
    method = "efron"
  } else {
    method = method
  }

  # trunc argument default
  if(missing(trunc)) {
    trunc = 0.95
  } else {
    trunc = trunc
  }

  # na.action argument default
  if(missing(na.action)) {
    na.action = NULL # need to check if na.action works for both
                     # coxph and coxr
  } else {
    na.action = na.action
  }

  # f.weight argument default
  if(missing(f.weight)) {
    f.weight = NULL
  } else {
    f.weight = f.weight
  }

  # weight argument default
  if(missing(weights)) { # is this the same as weight
    weights = NULL
  } else {
    weights = weights
  }

  # init argument default
  if(missing(init)) {
    init = NULL
  } else {
    init = init
  }

  # control argument default
  if(missing(control)) {
    control = NULL
  } else {
    control = control
  }

  # singular.ok argument default
  if(missing(singular.ok)) {
    singular.ok = NULL
  } else {
    singular.ok = singular.ok
  }

  # model argument default
  if(missing(model)) {
    model = NULL
  } else {
    model = model
  }

  # subset argument default
  if(missing(subset)) {
    subset = NULL
  } else {
    subset = subset
  }

  # Estimate IRR
  irr <- coxrobust::coxr(formula = formula, data = data, method = method,
                         trunc = trunc,
                         na.action = na.action, f.weight = f.weight,
                         singular.ok = singular.ok, model = model,
                         subset = subset)

  # Estimate PLM
  plm <- survival::coxph(formula = formula, data = data, method = method,
                         na.action = na.action, weights = weights,
                         singular.ok = singular.ok, model = model,
                         subset = subset,
                         init = init, control = control,
                         y = TRUE, x = TRUE) # using coxph() to format data
  surv <- plm$y
  x <- plm$x

  # Making empty vectors to prep for cross-validation
  n <- nrow(x)
  cvll_r <- numeric(n)
  cvll_c <- numeric(n)

  # Loop through for cross-validation
  for (i in 1:n){
    # Remove current observation
    survi <- surv[-i, ]
    xi <- as.matrix(x[-i, ])

    # Estimate models without current observation i
    pesti <- survival::coxph(survi ~ xi, method = method) ### plm - rename to something more descriptive
    esti <- coxrobust::coxr(survi ~ xi, trunc = trunc)

    # Check if any parameters were undefined without observation i
    # This can happen with very sparse and/or very many covariates
    # (e.g., fixed effect dummies)
    na_ind <- which(is.na(pesti$coefficients))

    # Extract coefficients and covariates
    coef_p <- pesti$coefficients
    x_p <- x # unrestricted model???? is that what we could call it
    xi_p <- xi

    # Remove any covariates with undefined effects
    if (length(na_ind) > 0){
      coef_p <- pesti$coefficients[-na_ind]
      x_p <- x[, -na_ind]
      xi_p <- xi[, -na_ind]
    }

    # Compute the full and restricted partial likelihoods
    full_ll_r <- survival::coxph(surv ~ offset(as.matrix(x) %*% cbind(esti$coefficients)),
                       method = method)$loglik # this is unrestricted - fix this _r and _c
    full_ll_c <- survival::coxph(surv ~ offset(as.matrix(x_p) %*% cbind(coef_p)),
                       method = method)$loglik # this is unrestricted with nas dropped - fix this _r and _c
    esti_ll_r <- survival::coxph(survi ~ offset(as.matrix(xi) %*% cbind(esti$coefficients)),
                       method = method)$loglik
    esti_ll_c <- survival::coxph(survi ~ offset(as.matrix(xi_p) %*% cbind(coef_p)),
                       method = method)$loglik
    ### getting the likelihood - so just doesn't to be irr
    ### offset() is forcing it to that beta - that linear predictor

    # Store
    cvll_r[i] <- full_ll_r - esti_ll_r ## why are we leaving out observations???
    cvll_c[i] <- full_ll_c - esti_ll_c
  }

  # Compute the test
  cvmf <- binom.test(sum(cvll_r > cvll_c), n, alternative = "two.sided")
  best <- ifelse(cvmf$statistic > n / 2, "IRR", "PLM")
    ## binomial test - null is just fair coin, n/2
  p <- round(cvmf$p.value, digits = 3)
  res_sum <- cat(best, " supported with a two-sided p-value of ",
                 p, sep = "", "\n") # need to cut this out for loops

  # Construct the returned object
  obj <- list(res_sum = res_sum, cvmf = cvmf,
              cvpl_irr = cvll_r, cvpl_plm = cvll_c,
              irr = irr)
  res_sum
  obj
}
