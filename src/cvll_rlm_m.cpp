//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

arma::colvec vecrobustm(Rcpp::List a) {
  int size = a.size();
  arma::colvec a1(size);
  for (int i = 0; i < size; i++) {
    a1[i] = a[i];
  }
  return a1;
}

Rcpp::List gam(double &x) {
  Rcpp::Environment stats("package:stats");
  Rcpp::Function f = stats["gamma"];
  return f(x);
}

Rcpp::List robustm(arma::dmat &x, arma::vec &y) {
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("modeLLtest");
  // Added this to package since trouble pulling from MASS
  Rcpp::Function f = pkg["rlm_m"];
  return f(x, y);
}

// [[Rcpp::export]]
Rcpp::List cvll_rlm_m(arma::dmat &x, arma::colvec &y, int n_row, int n_col) {

  int n = n_row - 1;
  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  Rcpp::List rlm;
  arma::colvec coef;
  arma::colvec resid;
  double sig;
  Rcpp::List cvll_rlm(n_row);

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i); // define obs i before change y
    rowyi = y.row(i);
    y.shed_row(i); // leaves out observation i
    xv = x.row(i); // define obs i before change x
    rowxi = x.row(i);
    x.shed_row(i); // leaves out observation i but changes x
    rlm = robustm(x, y);
    coef = vecrobustm(rlm("coefficients"));
    resid = vecrobustm(rlm("residuals")); // residuals
    sig = arma::as_scalar( sqrt(arma::trans(resid) * resid /  (n - n_col) )); // sqrt(SE of est)
    cvll_rlm[i] = gam( ((n - n_col) + 1) / 2 ) /
      ( sig * sqrt( (n - n_col) * M_PI ) *
      gam( (n - n_col) / 2 ) ) *
      (((n - n_col) + (resid * resid) / (sig * sig) ) /
      (n - n_col) ) ^ ( - ((n - n_col) + 1) / 2);
    y.insert_rows(i, rowyi); // add y back in
    x.insert_rows(i, rowxi); // add x back in
  }

  return cvll_rlm;
}
