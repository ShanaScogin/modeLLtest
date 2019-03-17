//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <math.h> // for log
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List cvll_ols(arma::dmat &x, arma::mat &y, int n_row, int n_col) {

  int n = n_row - 1;
  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  arma::dmat coef;
  arma::colvec resid;
  double sig2;
  List cvll_ls(n_row);

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i); // define obs i before change y
    rowyi = y.row(i);
    y.shed_row(i); // leaves out observation i
    xv = x.row(i); // define obs i before change x
    rowxi = x.row(i);
    x.shed_row(i); // leaves out observation i but changes x
    coef = arma::solve(x, y); // fit model y ~ x
    resid = y - x * coef; // residuals
    sig2 = arma::as_scalar( arma::trans(resid) * resid /  (n - n_col) ); // SE of est
    cvll_ls[i] = log(arma::normpdf(yv - xv * coef, 0, sig2));
    y.insert_rows(i, rowyi); // add y back in
    x.insert_rows(i, rowxi); // add x back in
  }

  return cvll_ls;
}

