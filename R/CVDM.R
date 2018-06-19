### Functions for the Computation of Vuong and the Cross-Validated Johnson's t-test with OLS and MR ###

# Create function to compute skewness ---------------------------
mu3hat <- function(x){
  n <- length(x)
  ns <- n * 1 / (n - 1) * 1 / (n - 2)
  ns * sum( (x - mean(x) )^3)
}

# Create function for Johnson's t ---------------------------
johnsons_t <- function(x){
  m3 <- mu3hat(x)
  s <- sd(x)
  n <- length(x)
  (mean(x) + m3 / (6 * s^2 * n) + m3 / (3 * s^4) * mean(x)^2) * sqrt(n) / s
}

# Create function for centered Laplace density ---------------------------
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

# lls for MR and OLS ---------------------------
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

# cvlls for MR and OLS ---------------------------
cvll2 <- function(formula, data){
  # want to take this out: devtools::use_package("MASS")
  # want to take this out: devtools::use_package("quantreg")
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

# Create function for computing the CVJT ---------------------------
## Takes formula and data frame arguments
## Returns cvjt and vuong -- respective t-statistics, can be used as t or z stats.
## both tests are fit(OLS)-fit(MR), such that negative values suport MR
CVDM <- function(formula, data){
  # want to take this out: devtools::use_package("quantreg")
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
