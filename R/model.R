
model <- function(object, ...) UseMethod("model")

print.model <- function(x, ...)
{
  cat("\nModel:", x$model, "\n")

  invisible(x)
}

# cross-validated log likelihoods

OLS <- function(x, y){
  cvll <- NA # empty vector for OLS cvlls
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
    sig <- summary(ls)$sigma # dispersion parameter
    cvll[i] <- dnorm(yv - rbind(xv) %*% coef(ls), sd = sig, log = TRUE)
  }
  return(list(cvll = cvll, # put structure() instead of return()?
              n = length(y), # number of observations
              m = (length(y) - length(cvll)))) # num of missing obs
}

MR <- function(x, y){
  cvll <- NA # empty vector for MR cvlls
  for (i in 1:length(y)){
    yt <- y[-i] # leaves out observation i
    if (!is.matrix(try(solve(t(x) %*% x)))) { # returns TRUE if inv X'X does not exist
      next # skips to next iteration
    } else {
      xt <- x[-i, ] # if inverse X'X exists, creates object
    }
    yv <- y[i]
    xv <- x[i, ]
    mr <- quantreg::rq(yt ~ -1 + xt)
    b <- mean(abs(residuals(mr))) # dispersion parameter
    cvll[i] <- log(dlapl(yv - rbind(xv) %*% coef(mr), b = b))
  }
  return(list(cvll = cvll, # put structure() instead of return()?
              n = length(y), # number of observations
              m = (length(y) - length(cvll)))) # num of missing obs
}
