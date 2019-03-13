//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <iostream> // for cout debugging
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
arma::colvec cvll_ols(arma::dmat x, arma::mat y, int n_row, int n_col) {

//  NumericVector cvll_ls(n_row) ;
  arma::rowvec rowxi ;
  arma::rowvec rowyi ;
  arma::dmat yv ;
  arma::dmat xv ;
  arma::colvec coef ;
  double sig2 ;
  arma::colvec resid ;
  int n = n_row - 1 ;

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i) ; // defining obs i before change y
    rowyi = y.row(i) ;
    y.shed_row(i) ; // leaves out observation i
    xv = x.row(i) ; // defining obs i before change x
    rowxi = x.row(i) ;
    x.shed_row(i) ; // leaves out observation i but changes x
//    cout << x << endl ;
    coef = arma::solve(x, y) ; // fit model y ~ X - includes intercept
    resid = y - x * coef; // residuals
    sig2 = arma::as_scalar( arma::trans(resid)*resid/(n - n_col) ); // SE of est
//    cvll_ls[i] = R::dnorm(yv - R::rbind(xv) %*% ls,
//               sd = sig, log = TRUE) ;
    y.insert_rows(i, rowyi) ; // adding rest of y back in
    x.insert_rows(i, rowxi) ; // adding rest of x back in
  }

  return resid ;
}

