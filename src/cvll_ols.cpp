#include <RcppArmadillo.h>
#include <iostream>
// #include <RcppEigen.h> // for arma::solve or fastLm or something? not sure if nec
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;
//using namespace arma;

// [[Rcpp::export]]
arma::dmat cvll_ols(arma::dmat x, arma::mat y, int n_row) {

//  NumericVector cvll_ls(n_row) ;
//  arma::dmat yt ;
//  arma::dmat xt ;
  arma::rowvec rowxi ;
  arma::rowvec rowyi ;
  arma::dmat yv ; // this is actually a double but cpp gets angry bc y is vector
  arma::dmat xv ; // should this be vector? it's vector of ivs
//  List ls ;
//  double sig ;

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i) ; // defining obs i before change y
    rowyi = y.row(i) ;
    y.shed_row(i) ; // leaves out observation i
    xv = x.row(i) ; // defining obs i before change x
    rowxi = x.row(i) ;
    x.shed_row(i) ; // leaves out observation i but changes x
//    cout << x << endl ;
//    ls = fastLm(xt, yt) ; // check the intercept here
//    sig = R::summary(ls)$sigma ; // dispersion parameter
//    cvll_ls[i] = R::dnorm(yv - R::rbind(xv) %*% R::coef(ls),
//               sd = sig, log = TRUE) ;
    y.insert_rows(i, rowyi) ; // adding rest of y back in
    x.insert_rows(i, rowxi) ; // adding rest of x back in
  }

  return x ;
}
