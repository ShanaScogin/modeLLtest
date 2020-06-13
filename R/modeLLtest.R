#' modeLLtest Overview
#'
#' modeLLtest has three main functions to implement cross validated log likelihood
#' tests. To use this package, decide which specification(s) of a model
#' and distributions you wish compare. The function cvdm()
#' compares the fits of one model specification between a median
#' regression and ordinary least squares. The function cvmf() compares between
#' the fits of one model specificiation between two estimations of a Cox model.
#' The function cvll() extracts the leave-one-out cross-validated log-likelihoods
#' from a method of estimating a formula.
#'
#' @docType package
#' @name modeLLtest
NULL
#> NULL

#' @importFrom stats binom.test coef dnorm lm model.extract
#' model.frame model.matrix model.response pchisq pnorm pt
#' residuals sd lm.wfit contr.helmert mad median
NULL

#' @importFrom quantreg rq.fit
NULL

#' @importFrom survival coxph
NULL

#' @importFrom coxrobust coxr
NULL

#' @importFrom MASS lqs rlm
NULL

#' @useDynLib modeLLtest
#' @importFrom Rcpp sourceCpp
NULL
