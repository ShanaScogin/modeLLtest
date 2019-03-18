//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;
using namespace std;

#include <Rcpp.h>
using namespace Rcpp ;


arma::colvec vec(List a){
  int size = a.size();
  arma::colvec a1(size);
  for (int i = 0; i < size; i++) {
  a1[i] = a[i];
  }
  return a1;
}


// [[Rcpp::export]]
List cvll_mr(arma::dmat &x, arma::colvec &y, int n_row) {

  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  List mr;
  arma::colvec coef;
  arma::colvec resid;
  double b;
  List cvll_mr(n_row);

  Rcpp::Function rq = Environment("package:quantreg")["rq.fit"];

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
    b = arma::as_scalar( mean(abs(resid)) ); // dispersion param
    cvll_mr[i] = log( (1 / (2 * b) ) *
      exp( -abs( (yv - xv * coef) / b ) ) );
    y.insert_rows(i, rowyi); // add y back in
    x.insert_rows(i, rowxi); // add x back in
  }

  return cvll_mr;
}
