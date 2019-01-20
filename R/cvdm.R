#'This function implements the cross-validated difference in means
#'(CVDM) test. The function cvdm() tests between
#'Ordinary Least Squares (OLS) and 'Median Regression (MR). It returns
#'the Cross Validated Johnson's Test (CVJT). MORE about why and how.
#'For this to work properly,
#'the arguments MORE.
#'
#'@title Cross-Validated Difference in Means (CVDM) Test
#'@description Applies cross-validated log-likelihood to test if
#'a regression toward the mean - Ordinary Least Squares (OLS) - or a
#'median regression (MR) is a more appropriate model.
#'@param formula An object of class "formula" (or one that can be coerced
#'to that class): a symbolic description of the model to be fitted. The
#'details of model specification are given under 'Details.'
#'@param data A data frame, list or environment (or object coercible by
#'as.data.frame to a data frame) containing the variables in the model.
#'@return An object to test whether a regression toward the mean, or
#'Ordinary Least Squares (OLS), or median regression (MR) is more appropriate. The object is the
#'Cross-Validated Johnson's t-test. A positive test statistics support OLS
#' and a negative test statistics support MR.
#' @export

cvdm <- function(formula, data){

  ## include singular.ok??? - right now skips singular
  ## include na.action?? - right now cannot handle NAs
  ## include subset (easy)

  call <- match.call() # this doesn't do anything now -will add more when add arguments

  model <- lm(formula, data = data) # using lm() for rank - consider redoing
  cvlls <- cvloglikes(formula, data)
  cvlldiff <- cvlls[[1]] - cvlls[[2]] # cross-validated log likelihood difference
  test_stat <- johnsons_t(cvlldiff)
  p_value <- ifelse (test_stat > 0,
                    pt(test_stat, df = nrow(data) - model$rank, # student t distrib
                       lower.tail = FALSE),
                    pt(test_stat, df = nrow(data) - model$rank)) # student t distrib
  # Positive test statistics support OLS
  # Negative test statistics support MR
  best <- ifelse(test_stat > 0, "OLS", "MR")
  obj <- list(best = best,
              test_stat = test_stat,
              p_value = p_value,
              n = cvlls[1],
              ols_stat = cvlls[2],
              mr_stat = cvlls[3],
              missing_obs = cvlls[4],
              call = call)

  class(obj) <- "cvdm"

  obj

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

  mf <- model.frame(formula = formula, data = data)
  x <- model.matrix(attr(mf, "terms"), data = mf)
  y <- model.response(mf)

  cvll_ls <- NA # empty vector for OLS cvlls
  cvll_mr <- NA # empty vector for MR cvlls

  for (i in 1:length(y)){
    yt <- y[-i] # leaves out observation i
    if (!is.matrix(try(solve(t(x) %*% x)))) { # returns TRUE if inv X'X does not exist
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
      # dnorm() sets mean = 0
      # so dnorm(y = xBeta, mean = 0, sd = sigma) =
      # dnorm(y, mean = xBeta, sd = sigma)
    cvll_mr[i] <- log(dlapl(yv - rbind(xv) %*% coef(mr), b = b))
  }

  return(list(LS = cvll_ls,
              MR = cvll_mr,
              n = length(y), # number of observations
              m = (length(y) - length(cvll_ls)))) # number of missing observations

}
