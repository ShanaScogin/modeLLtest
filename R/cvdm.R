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
#'@param cvdm The objects returned by computing the cross-validated difference
#'in means test (CVDM). Uses rq() function from quantreg package and lm() from
#'base R. Inputs include the model formula and the data as a dataframe.
#'Uses the object from the johnsons_t() function and the cvloglikes()
#'function. Negative values suport MR.
#'@param mu3hat The object returned by the estimation of skewness. It is
#'used in the estimation of the Johnson's t-test. The input is the
#'object cvloglikes (cross-validated log likelihood difference) in the
#'cvdm() function.
#'@param johnsons_t The object returned by the t-statistic adjustment for
#'skewness using the procedure suggested by Johnson (1978). Input is the
#'object cvlldiff (cross-validated log likelihood difference) within the
#'cvdm() function. It is estimated using the mu3hat object and used to
#'compute the CVDM test.
#'@param dlapl The object returned by the estimation of the centered Laplace
#'density. This is the Laplace equivlant of dnorm() for the normal
#'distribution. It is used in the estimation of the cvloglikes object.
#'@param cvloglikes The object returned by the estimation of the
#'cross-validated log likelihood for both the OLS and MR estimations.
#'Inputs include the model formula and the data as a dataframe.
#'If the inverse of the X'X matrix does not exist for one or more observations,
#'returned object includes an error warning. Uses rq() function from quantreg
#'package and lm() from base R package. Also uses the object returned from
#'dlapl(). Object is used to construct the CVDM test.
#'@return A function for the computation of the Cross-Validated
#'Johnson's t-test to test between OLS and MR.

cvdm <- function(formula, data){
  model <- lm(formula, data = data)
  cvlls <- cvloglikes(formula, data)
  cvlldiff <- cvlls[[1]] - cvlls[[2]] # cross-validated log likelihood difference
  test.stat <- johnsons_t(cvlldiff)
  p.value <- ifelse(test.stat > 0,
                    pt(test.stat, df = nrow(data) - model$rank, # student t distrib
                       lower.tail = FALSE),
                    pt(test.stat, df = nrow(data) - model$rank)) # student t distrib
  # Positive test statistics support OLS
  # Negative test statistics support MR
  return(list(test.stat = test.stat, p.value = p.value))
}

mu3hat <- function(x){
  n <- length(x)
  ns <- n / ((n - 1) * (n - 2)) # change object name?
  ns * sum( (x - mean(x) ) ^ 3) # why split
}

johnsons_t <- function(x){ # input is cross-validated log likelihood difs
  m3 <- mu3hat(x)
  s <- sd(x)
  n <- length(x)
  (mean(x) + m3 / (6 * s ^ 2 * n) + m3 / (3 * s ^ 4) * mean(x) ^ 2) * sqrt(n) / s
}

dlapl <- function(x, b){
  return(1 / (2 * b) * exp(-abs(x / b)))
}

cvloglikes <- function(formula, data){ # cross-validated log likelihoods
  cvll_ls <- numeric(length(y)) # empty vector for OLS cvlls
  cvll_mr <- numeric(length(y)) # empty vector for MR cvlls
  singular_count <- 0 # empty vector to count missing observations
  est <- lm(formula, data = data,
            x = TRUE,
            y = TRUE) # using lm() to format data
  x <- est$x
  y <- est$y
  for (i in 1:length(y)){
    yt <- y[-i] # leaves out observation i
    if (!is.matrix(try(solve(t(x) %*% x)))) { # returns TRUE if inv X'X does not exist
      singular_count <- singular_count + 1 # adds to counter if singular
      next # skips to next iteration
      } else {
        xt <- x[-i, ] # if inverse X'X exists, creates object
        }
    yv <- y[i]
    xv <- x[i, ]
    ls <- lm(yt ~ -1 + xt) # -1 takes out the intercept (1 is identifier)
    mr <- quantreg::rq(yt ~ -1 + xt)
    sig <- summary(ls)$sigma # dispersion parameter
    b <- mean(abs(residuals(mr))) # dispersion parameter
    cvll_ls[i] <- dnorm(yv - rbind(xv) %*% coef(ls), sd = sig, log = TRUE)
    cvll_mr[i] <- log(dlapl(yv - rbind(xv) %*% coef(mr), b = b))
  }
  return(list(LS = cvll_ls, MR = cvll_mr)) # add number of observations, maybe missing?
  if (singular_count > 0) {
    return("One or more observations were skipped") # code as a warning and add spec number
  }
}
