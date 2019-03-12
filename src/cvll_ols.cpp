/**
 * Calculates the values of the leave-one-out cross-validated
 * log-likelihood function for OLS
 * @parma the matrix of independent variables
 * @parma the vector of dependent variables
 * @parma the number of observations
 *
 * @return void
 */

#include <math.h> // not sure if needed
#include "R.h" // not sure if needed
#include <RcppArmadillo.h>
#include <RcppEigen.h> // for arma::solve
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std; //not sure if needed
using namespace arma;

// [[Rcpp::export}}]]

NumericVector cvll_ols(NumericMatrix *x, NumericVector *y, int *n_row) { //dmatrix for x?
// need to add nrow to r file wrapper function

  NumericVector cvll_ls ;
  NumericVector yt ;
  NumericMatrix xt ;
  NumericVector yv ; // this is actually a double but cpp gets angry bc y is vector
  NumericMatrix xv ; // should this be vector? it's vector of ivs
  double ls ;
  double sig ;

  // resource: https://teuder.github.io/rcpp4everyone_en/100_matrix.html
  for (int i = 0; i <* n_row; i++) { //check <* rather than <
    yt = y[-i] ; // leaves out observation i for out of sample
    xt =  x[-i] ; // leaves out observation i for out of sample
    yv = y[i] ; // obs i
    xv = x[-i] ; // obs i
    ls = fastLm(xt, yt) ; // check the intercept here
//    sig = R::summary(ls)$sigma ; // dispersion parameter
//    cvll_ls[i] = R::dnorm(yv - R::rbind(xv) %*% R::coef(ls),
//               sd = sig, log = TRUE) ;
  }

  return cvll_ls ;
}
