#################################################################
# The functions contained in this file implements               #
# the cross-validated median fit (CVMF) test                    #
# in Desmarais and Harden (Forthcoming, Political Analysis).    #
# The computation of this test statistic may                    #
# take several minutes. Given N observations in                 #
# the data, the Cox model must be estimated N                   #
# times by both PLM and IRR (2N estimations).                   #
#################################################################


CVMF <- function(formula, data, method = "breslow", trunc = 0.95){
  ## "formula" is a standard R formula representation of the model to be
  # estimated, y ~ x1 + x2 + ... + xk. Note that y must be a "Surv" object
  ## "data" is the data frame containing the variables
  ## "method" is the algorithm for handling ties in the PLM. Note that
  # only the Breslow method is available in the coxrobust implementation of IRR.
  ## "trunc" the proportion of observations to recveive positive weight in
  # the next round of IRR (i.e., M in Desmarais and Harden (XXXX)).

  # Load standard survival analysis package
  devtools::use_package("survival")
  # Load package for IRR
  devtools::use_package("coxrobust")

  # Estimate PLM
  plm <- coxph(formula, data = data, method = method, y = TRUE, x = TRUE)
  surv <- plm$y
  x <- plm$x

  # Estimate IRR
  irr <- coxr(formula, data = data, trunc = trunc)

  # Prep for cross-validation
  n <- nrow(x)
  cvll_r <- numeric(n)
  cvll_c <- numeric(n)

  # Loop through for cross-validation
  for (i in 1:n){
    # Remove current observation
    ind <- i
    survi <- surv[-ind, ]
    xi <- as.matrix(x[-ind, ])

    # Estimate models without i
    esti <- coxr(survi ~ xi, trunc = trunc)
    pesti <- coxph(survi ~ xi, method = method)

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


    # Compute the full and restrcicted partial likelihoods
    full_ll_r <- coxph(surv ~ offset(as.matrix(x) %*% cbind(esti$coefficients)),
                       method = method)$loglik
    full_ll_c <- coxph(surv ~ offset(as.matrix(x_p) %*% cbind(coef_p)),
                       method = method)$loglik
    esti_ll_r <- coxph(survi ~ offset(as.matrix(xi) %*% cbind(esti$coefficients)),
                       method = method)$loglik
    esti_ll_c <- coxph(survi ~ offset(as.matrix(xi_p) %*% cbind(coef_p)),
                       method = method)$loglik

    # Store
    cvll_r[i] <- full_ll_r - esti_ll_r
    cvll_c[i] <- full_ll_c - esti_ll_c
  }

  # Compute the test
  cvmf <- binom.test(sum(cvll_r > cvll_c), n, alternative = "two.sided")
  best <- ifelse(cvmf$statistic > n / 2, "IRR", "PLM")
  p <- round(cvmf$p_value, digits = 3)
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

### Toy Example. Delete the "#" before each line to run ##
## Read in survival library
# require(survival)
#
## Set the seed for replication purposes
# set.seed(12345)
#
# Create two covariates with measurement error in the second
# x1 <- rnorm(100)
# x2 <- rnorm(100)
# x2e <- x2 + rnorm(100, 0, 0.5)

## Create the dependent variable with the unobserved x2 (no measurement error)
## Each coefficient has a true value of 1
# y <- rexp(100, exp(x1 + x2))
# y <- Surv(y)
#
## Put the observed variables into a data frame
# dat <- data.frame(y, x1, x2e)
#
## Define the formula
# form <- y ~ x1 + x2e
#
# results <- CVMF(formula = form, data = dat)

#
## Take a look at results
# results$irr
## Now the test
# results$cvmf
