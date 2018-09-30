#'Create function to compute the Cross-Validated Difference in Means (CVDM) Test
#'
#'\code{CVDM} returns the cross-validated difference in means test
#'
#'The functions contained in this file implement the cross-validated
#'difference in means (CVDM) test. The function cvdm() tests between
#'Ordinary Least Squares (OLS) and 'Median Regression (MR). It returns
#'the Cross Validated Johnson's Test (CVJT).For this to work properly,
#'the arguments MORE.
#'
#'@title A function to compute the Cross-Validated Difference in Means Test.
#'@description One of the main functions of the package.
#'Applies cross-validated log-likelihood to test between OLS and MR.
#'@param cvdm The objects returned by computing the cross validated Johnson's
#'t-test (CVJT). MORE ABOUT WHAT OBJECTS IT USES. Negative values suport MR.
#'@param mu3hat The object returned by the estimation of skewness. It is
#'used in the estimation of the Johnson's t-test.
#'@param johnsons_t The object returned by the t-statistic adjustment for
#'skewness using the procedure suggested by Johnson (1978). It is estimated
#'using the mu3hat object and used to compute the CVDM test.
#'@param dlapl The object returned by the estimation of the centered Laplace
#'  density. It is used in the estimation of the loglikes and cvloglikes objects.
#'@param desingular The object returned by the estimation of MORE BOUT MATRIX.
#'It is used in the estimation of the cvloglikes objects.
#'@param loglikes The object returned by the estimation of the individual
#'likelihood (or log likelihood). It is used in the construction of the CVDM test.
#'@param cvloglikes The object returned by the estimation of the
#'cross-validated log likelihood. It is estimated using the desingular object
#'and is used to construct the CVDM test.
#'@return A function for the computation of Vuong and the Cross-Validated
#'Johnson's t-test to test between OLS and MR

# for later: Instead of including examples directly in the documentation, you can put
# them in separate files and use @example path/relative/to/package/root to insert
# them into the documentation.

cvdm <- function(formula, data){
  model <- lm(formula, data = data)
  lls <- loglikes(formula, data)
  cvlls <- cvloglikes(formula, data)
  lld <- lls[[1]] - lls[[2]] # log likelihood difference
  cvlld <- cvlls[[1]] - cvlls[[2]] # cross-validated log likelihood difference
  test_stat <- johnsons_t(cvlld)
  p_value <- ifelse(test_stat > 0,
                    pt(test_stat, df = nrow(data) - model$rank, # student t distrib
                       lower.tail = FALSE),
                    pt(test_stat, df = nrow(data) - model$rank)) # student t distrib
  # Positive test statistics support OLS
  # Negative test statistics support MR
  return(list(test_stat = test_stat, p_value = p_value))
}

mu3hat <- function(x){
  n <- length(x)
  ns <- n / ((n - 1) * (n - 2)) # change object name?
  ns * sum( (x - mean(x) ) ^ 3) # why split
}

johnsons_t <- function(x){ # x is the cross-validated log likelihood differences
  m3 <- mu3hat(x)
  s <- sd(x)
  n <- length(x)
  (mean(x) + m3 / (6 * s ^ 2 * n) + m3 / (3 * s ^ 4) * mean(x) ^ 2) * sqrt(n) / s
}

dlapl <- function(x, b){
  return(1 / (2 * b) * exp(-abs(x / b)))
}

desingular <- function(x, y){ # add explanatory error message here?
                              # better object name here?
  inv <- try(solve(t(x) %*% x))
  if (!is.matrix(inv)){
    sdy <- sd(y)
    for (i in 1:ncol(x)){
      sdx <- sd(x[, i])
      if (sdx > 0) x[, i] <- x[, i] + rnorm(length(x[, i]), sd = sdx / 10000)
      if (sdx == 0) x[, i] <- x[, i] + rnorm(length(x[, i]), sd = sdy / 10000)
    }
  }
  return(x)
} ### This needs to change to throw the missing obs out or something

loglikes <- function(formula, data){ # log likelihood estimations of OLS and MR
  ls <- lm(formula, data = data) # OLS model
  mr <- quantreg::rq(formula, data = data) # MR model
  sig <- summary(ls)$sigma # dispersion parameter for OLS
  b <- mean(abs(residuals(mr))) # dispersion parameter for MR
  ll_ls <- dnorm(residuals(ls), sd = sig,
                 log = TRUE)
  ll_mr <- log(dlapl(residuals(mr), b = b))
  return(list(LS = ll_ls, MR = ll_mr))
}

cvloglikes <- function(formula, data){ # cross-validated log likelihoods
  est <- lm(formula, data = data,
            x = TRUE,
            y = TRUE)
  x <- est$x
  y <- est$y
  cvll_ls <- numeric(length(y))
  cvll_mr <- numeric(length(y))
  for (i in 1:length(y)){ # figure out another way besides for loop
    yt <- y[-i]
    xt <- desingular(x[-i, ], yt)
    yv <- y[i]
    xv <- x[i, ]
    ls <- lm(yt ~ -1 + xt)
    mr <- quantreg::rq(yt ~ -1 + xt)
    sig <- summary(ls)$sigma
    b <- mean(abs(residuals(mr)))
    cvll_ls[i] <- dnorm(yv - rbind(xv) %*% coef(ls), sd = sig, log = TRUE)
    cvll_mr[i] <- log(dlapl(yv - rbind(xv) %*% coef(mr), b = b))
  }
  return(list(LS = cvll_ls, MR = cvll_mr))
}
