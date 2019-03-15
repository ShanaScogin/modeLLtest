#' dontrun{
#'cvll_mr <- function(x, y){ # cross-validated log likelihoods
#'#  mf <- model.frame(formula = formula, data = data)
#'#  x <- model.matrix(attr(mf, "terms"), data = mf)
#'#  y <- model.response(mf)
#'  cvll_mr <- NA # empty vector for OLS cvlls
#'  for (i in 1:length(y)){
#'    yt <- y[-i] # leaves out observation i
#'    if (!is.matrix(try(solve(t(x) %*% x)))) { # returns TRUE if inv X'X does not exist
#'      next # skips to next iteration
#'      } else {
#'        xt <- x[-i, ] # if inverse X'X exists, creates object
#'        }
#'    yv <- y[i]
#'    xv <- x[i, ]
#'    mr <- quantreg::rq(yt ~ -1 + xt) # -1 takes out the intercept (1 is identifier)
#'    b <- mean(abs(residuals(mr))) # dispersion parameter
#'    cvll_mr[i] <- log(dlapl(yv - rbind(xv) %*% coef(mr), b = b))
#'  }
#'
#'  return(list(MR = cvll_mr,
#'              n = length(y), # number of observations
#'              m = (length(y) - length(cvll_mr)))) # number of missing observations
#'}
#'
#'dlapl <- function(a, b){
#'  return(1 / (2 * b) * exp(-abs(a / b)))
#'}
#'}
