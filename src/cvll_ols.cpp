#include <RcppArmadillo.h>
#include <iostream>
// #include <RcppEigen.h> // for arma::solve or fastLm or something? not sure if nec
//[[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace std;
//using namespace arma;

// [[Rcpp::export]]
arma::dmat cvll_ols(arma::dmat x, NumericVector y, int n_row) {

//  int n_row = y.size() ;
//  NumericVector cvll_ls(n_row) ;
  arma::dmat yt ; //change back to numericvector
  arma::dmat xt ;
  NumericVector yv(1) ; // this is actually a double but cpp gets angry bc y is vector
  NumericVector xv ; // should this be vector? it's vector of ivs
//  List ls ;
//  double sig ;

  // resource: https://teuder.github.io/rcpp4everyone_en/100_matrix.html
  for (int i = 0; i < n_row; i++) {
//    yt = y.shed_row(i) ; // leaves out observation i
    arma::rowvec rowi = x.row(i) ;
    x.shed_row(i) ; // leaves out observation i
//    cout << x << endl ;
//    yv = y[i] ; // obs i
//    xv = x.row(i) ; // obs i
//    ls = fastLm(xt, yt) ; // check the intercept here
//    sig = R::summary(ls)$sigma ; // dispersion parameter
//    cvll_ls[i] = R::dnorm(yv - R::rbind(xv) %*% R::coef(ls),
//               sd = sig, log = TRUE) ;
    x.insert_rows(i, rowi) ;
  }

  return x ;
}
