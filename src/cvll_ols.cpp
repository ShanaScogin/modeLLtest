//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <iostream>
#include <RcppEigen.h>
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List cvll_ols(arma::dmat x, arma::mat y, int n_row) {

//  NumericVector cvll_ls(n_row) ;
  arma::rowvec rowxi ;
  arma::rowvec rowyi ;
  arma::dmat yv ; // this is actually a double but cpp gets angry bc y is vector
  arma::dmat xv ; // should this be vector? it's vector of ivs
  List ls ;
//  double sig ;

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i) ; // defining obs i before change y
    rowyi = y.row(i) ;
    y.shed_row(i) ; // leaves out observation i
    xv = x.row(i) ; // defining obs i before change x
    rowxi = x.row(i) ;
    x.shed_row(i) ; // leaves out observation i but changes x
//    cout << x << endl ;
//    ls = arma::solve(x, y) ; // this includes intercept
    ls = Eigen::fastLm(x, y) ; // this includes intercept
//    ls = 1 / ((arma::inplace_trans(x) * x) * (arma::inplace_trans(x) * y))
//    sig = arma::inplace_trans(y - (x * ls)) * y - (x * ls)) / (n_row - n_col)
//    sig = arma::mean(abs(residuals(ls))) // dispersion parameter
//the square root of the estimated variance of the random error
//    cvll_ls[i] = R::dnorm(yv - R::rbind(xv) %*% ls,
//               sd = sig, log = TRUE) ;
    y.insert_rows(i, rowyi) ; // adding rest of y back in
    x.insert_rows(i, rowxi) ; // adding rest of x back in
  }

  return ls ;
}

