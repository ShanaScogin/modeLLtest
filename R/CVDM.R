# old name: Create function to compute Vuong and the Cross-Validated Johnson's
# t-test vuong constitutes a z-test of the null hypothesis that E[δ] = 0 why is
# this computing vuong? cvjt is nec because it is possible that there is
# significant skew in the distribution of δ(cv), the t-statistic is adjusted for
# skewness using the procedure suggested by Johnson (1978) so this test just
# combines the two

#'Create function to compute the Cross-Validated Difference in Means Test
#'
#'\code{CVDM} returns the cross-validated difference in means test
#'
#'These are functions for the computation of Vuong and the Cross-Validated
#'Johnson's t-test. The main CVDM function tests between OLS and MR and returns
#'the CVJT and Vuong test -- respective t-statistics, can be used as t or z
#'stats. Both tests are fit(OLS)-fit(MR), such that negative values support MR.
#'For this to work properly, the arguments MORE.
#'
#'@title A function to compute the cross-validated difference in means test
#'  (CVDM).
#'@description One of the main functions provided by the package.
#'@param CVDM The objects returned by computing the cross validated Johnson's
#'  t-test (CVJT). #Check - why not Vuong here? does it compute this? Returns
#'  CVJT and Vuong -- respective t-statistics, can be used as t or z stats.
#'  Takes formula and data frame arguments. MORE ABOUT WHAT OBJECTS IT USES.
#'  Both tests are fit(OLS)-fit(MR), such that negative values suport MR.
#'@param mu3hat The object returned by the estimation of skewness. It is used in
#'  the estimation of the Johnson's t-test.
#'@param johnsons_t The object returned by the t-statistic adjustment for
#'  skewness using the procedure suggested by Johnson (1978). It is estimated
#'  using the mu3hat object and used to compute the CVDM test.
#'@param dlapl The object returned by the estimation of the centered Laplace
#'  density. It is used in the estimation of the ll2 and cvll2 objects.
#'@param desingular The object returned by the estimation of MORE BOUT MATRIX.
#'  It is used in the estimation of the cvll2 object.
#'@param ll2 The object returned by the estimation of the individual likelihood
#'  (or log likelihood). It is used in the construction of the CVDM test.
#'@param cvll2 The object returned by the estimation of the cross-validated log
#'  likelihood. It is estimated using the desingular object and is used to
#'  construct the CVDM test.
#'@return A function for the computation of Vuong and the Cross-Validated
#'  Johnson's t-test to test between OLS and MR

# for later: Instead of including examples directly in the documentation, you can put
# them in separate files and use @example path/relative/to/package/root to insert
# them into the documentation.

CVDM <- function(formula, data){
  model <- lm(formula, data = data)
  lls <- ll2(formula, data)
  cvlls <- cvll2(formula, data)
  lld <- lls[[1]] - lls[[2]]
  cvlld <- cvlls[[1]] - cvlls[[2]]
  test_stat <- johnsons_t(cvlld)
  p_value <- ifelse(test_stat > 0,
                    pt(test_stat, df = nrow(data) - model$rank,
                       lower.tail = FALSE),
                    pt(test_stat, df = nrow(data) - model$rank))
  cat("Positive test statistics support OLS",
      "\n",
      "Negative test statistics support MR", "\n")
  return(list(test_stat = test_stat, p_value = p_value))
}

# Need to fix this return message - it's pretty long and annoying

mu3hat <- function(x){
  n <- length(x)
  ns <- n * 1 / (n - 1) * 1 / (n - 2)
  ns * sum( (x - mean(x) ) ^ 3)
}

johnsons_t <- function(x){
  m3 <- mu3hat(x)
  s <- sd(x)
  n <- length(x)
  (mean(x) + m3 / (6 * s ^ 2 * n) + m3 / (3 * s ^ 4) * mean(x) ^ 2) * sqrt(n) / s
}

dlapl <- function(x, b){
  return(1 / (2 * b) * exp(-abs(x / b)))
}

desingular <- function(x, y){
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
}

ll2 <- function(formula, data){
  ls <- lm(formula, data = data)
  mr <- quantreg::rq(formula, data = data)
  sig <- summary(ls)$sigma
  b <- mean(abs(residuals(mr)))
  ll_ls <- dnorm(residuals(ls), sd = sig,
                 log = TRUE)
  ll_mr <- log(dlapl(residuals(mr), b = b))
  return(list(LS = ll_ls, MR = ll_mr))
}

cvll2 <- function(formula, data){
  est <- lm(formula, data = data,
            x = TRUE,
            y = TRUE)
  x <- est$x
  y <- est$y
  cvll_ls <- numeric(length(y))
  cvll_mr <- numeric(length(y))
  for (i in 1:length(y)){
    yt <- y[-i]
    xt <- desingular(x[-i, ], yt)
    yv <- y[i]
    xv <- x[i, ]
    ls <- lm(yt ~ -1 + xt)
    mr <- rq(yt ~ -1 + xt)
    sig <- summary(ls)$sigma
    b <- mean(abs(residuals(mr)))
    cvll_ls[i] <- dnorm(yv - rbind(xv) %*% coef(ls), sd = sig, log = TRUE)
    cvll_mr[i] <- log(dlapl(yv - rbind(xv) %*% coef(mr), b = b))
  }
  return(list(LS = cvll_ls, MR = cvll_mr))
}
