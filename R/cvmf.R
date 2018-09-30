#'Create function to compute the Cross-Validated Median Fit Test
#'
#'\code{cvmf} returns the cross-validated median fit test
#'
#'The functions contained in this file implement the cross-validated
#'median fit (CVMF) test. The function cvmf() tests between the partial
#'likelihood maximization (PLM) and the iteratively reweighted robust
#'(IRR) method of estimation for a given application of the Cox model.
#'
#'@title A function to compute the cross-validated median fit test
#'(CVMF).
#'@description One of the main functions of the package.
#'Applies cross-validated log-likelihood to test between PLM and IRR.
#'@param cvmf
#'@return A function for the computation of cross-validated median fit test
#' (CVMF)

cvmf <- function(formula, data, method = "breslow", trunc = 0.95){
  ## "formula" is a standard R formula representation of the model to be
  # estimated, y ~ x1 + x2 + ... + xk. Note that y must be a "Surv" object
  ## "data" is the data frame containing the variables
  ## "method" is the algorithm for handling ties in the PLM. Note that
  # only the Breslow method is available in the coxrobust implementation of IRR.
  ## "trunc" the proportion of observations to recveive positive weight in
  # the next round of IRR (i.e., M in Desmarais and Harden (2012)).

  # Estimate PLM
  plm <- survival::coxph(formula, data = data, method = method, y = TRUE, x = TRUE)
  surv <- plm$y
  x <- plm$x

  # Estimate IRR
  irr <- coxrobust::coxr(formula, data = data, trunc = trunc)

  # Prep for cross-validation
  # Making empty vectors
  n <- nrow(x)
  cvll_r <- numeric(n)
  cvll_c <- numeric(n)

  # Loop through for cross-validation
  for (i in 1:n){
    # Remove current observation
    ind <- i # why not just put -i in the below? ***************
    survi <- surv[-ind, ]
    xi <- as.matrix(x[-ind, ])

    # Estimate models without current observation i
    esti <- coxrobust::coxr(survi ~ xi, trunc = trunc) #********* switch with below irr
    pesti <- survival::coxph(survi ~ xi, method = method) ### plm - rename to something more descriptive

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
    ######### one of the above needs to be IRR right

    # Store
    cvll_r[i] <- full_ll_r - esti_ll_r ## why are we leaving out observations???
    cvll_c[i] <- full_ll_c - esti_ll_c
  }

  # Compute the test
  cvmf <- binom.test(sum(cvll_r > cvll_c), n, alternative = "two.sided")
  best <- ifelse(cvmf$statistic > n / 2, "IRR", "PLM")
  p <- round(cvmf$p.value, digits = 3)
  res_sum <- cat(best, " supported with a two-sided p-value of ",
                 p, sep = "", "\n")

  # Construct the returned object
  obj <- list(res_sum = res_sum, irr = irr, plm = plm, cvmf = cvmf,
              cvpl_irr = cvll_r, cvpl_plm = cvll_c)
  # "irr" is the IRR object from coxr()
  # "plm" is the PLM object from coxph()
  # "cvmf" is the CVMF - two.sided binomial test
  ## with higher values favoring IRR
  # "cvpl_irr" is the cross-validated partial likelihood for IRR
  # "cvpl_plm" is the cross-validated partial likelihood for PLM

  res_sum
  obj

}
