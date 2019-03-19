//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;


arma::colvec vec(List a){
  int size = a.size();
  arma::colvec a1(size);
  for (int i = 0; i < size; i++) {
  a1[i] = a[i];
  }
  return a1;
}


// [[Rcpp::export]]
List cvll_rlm(arma::dmat &x, arma::colvec &y, int n_row, int n_col) {

  int n = n_row - 1;
  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  List mr;
  arma::colvec coef;
  arma::colvec resid;
  double sig2;
  List cvll_rlm(n_row);

  Rcpp::Function rq = Environment("package:MASS")["rlm"];

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i); // define obs i before change y
    rowyi = y.row(i);
    y.shed_row(i); // leaves out observation i
    xv = x.row(i); // define obs i before change x
    rowxi = x.row(i);
    x.shed_row(i); // leaves out observation i but changes x
    mr = rq(x, y);
    coef = vec(mr("coefficients"));
    resid = vec(mr("residuals")); // residuals
    sig2 = arma::as_scalar( arma::trans(resid) * resid /  (n - n_col) ); // SE of est
    cvll_rlm[i] = log(arma::normpdf(yv - xv * coef, 0, sig2));
    y.insert_rows(i, rowyi); // add y back in
    x.insert_rows(i, rowxi); // add x back in
  }

  return cvll_rlm;
}
