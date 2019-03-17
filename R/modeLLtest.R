#' modeLLtest
#'
#' @section modeLLtest Cross Validated Log Likelihood test functions:
#' To use this package, decide which specification(s) of a model
#' and distributions you wish compare. The functions in this package
#' compare the fits of one model specification between a median
#' regression and ordinary least squares (cvdm()), between the fits
#' of one model specificiation between two estimationsof a Cox model
#' (cvmf()), and between two model specification and one distribution
#' (cvll()).
#'
#' @docType package
#' @name modeLLtest
NULL
#> NULL

#' @importFrom stats binom.test coef dnorm lm model.extract
#' model.frame model.matrix model.response pchisq pnorm pt
#' residuals sd
NULL

#' @importFrom quantreg rq.fit
NULL

#' @useDynLib modeLLtest
#' @importFrom Rcpp sourceCpp
NULL
#> NULL
# do I need to include imports from survival and coxrobust here?
