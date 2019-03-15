#' dontrun{
#' cvll_ols <- function(x, y){ # cross-validated log likelihoods
#' #  mf <- model.frame(formula = formula, data = data)
#' #  x <- model.matrix(attr(mf, "terms"), data = mf)
#' #  y <- model.response(mf)
#'   cvll_ls <- NA # empty vector for OLS cvlls
#'   for (i in 1:length(y)){
#'     yt <- y[-i] # leaves out observation i
#'     if (!is.matrix(try(solve(t(x) %*% x)))) { # returns TRUE if inv X'X does not exist
#'       next # skips to next iteration
#'       } else {
#'         xt <- x[-i, ] # if inverse X'X exists, creates object
#'         }
#'     yv <- y[i]
#'     xv <- x[i, ]
#'     ls <- lm(yt ~ -1 + xt) # -1 takes out the intercept (1 is identifier)
#'     sig <- summary(ls)$sigma # dispersion parameter
#'     cvll_ls[i] <- dnorm(yv - rbind(xv) %*% coef(ls), sd = sig, log = TRUE)
#'   }
#'
#'   return(list(LS = cvll_ls,
#'               n = length(y), # number of observations
#'               m = (length(y) - length(cvll_ls)))) # number of missing observations
#' }
#' }
