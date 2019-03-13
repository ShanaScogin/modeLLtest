/**
 * Calculates the values of the leave-one-out cross-validated
 * log-likelihood function for OLS
 * @parma the matrix of independent variables
 * @parma the vector of dependent variables
 * @parma the number of observations
 *
 * @return void
 */

#include <RcppArmadillo.h>
#include <math.h>
#include "R.h"
#include "R_ext/Memory.h"
// #include <RcppEigen.h> // for arma::solve or fastLm or something? not sure if nec
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export}}]]
NumericVector cvll_ols(NumericMatrix x, NumericVector y) { //dmatrix for x?

  int n_row = y.size() ;
//  NumericVector cvll_ls(n_row) ;
  NumericVector yt(n_row - 1) ;
  NumericMatrix xt ;
  NumericVector yv(1) ; // this is actually a double but cpp gets angry bc y is vector
  NumericMatrix xv ; // should this be vector? it's vector of ivs
//  List ls ;
//  double sig ;

  // resource: https://teuder.github.io/rcpp4everyone_en/100_matrix.html
  for (int i = 0; i < n_row; i++) { //check <* rather than <?
    yt = y[-i] ; // leaves out observation i for out of sample
    xt =  x[-i] ; // leaves out observation i for out of sample
    yv = y[i] ; // obs i
    xv = x[i] ; // obs i
//    ls = fastLm(xt, yt) ; // check the intercept here
//    sig = R::summary(ls)$sigma ; // dispersion parameter
//    cvll_ls[i] = R::dnorm(yv - R::rbind(xv) %*% R::coef(ls),
//               sd = sig, log = TRUE) ;
  }

  return yv ;
}
