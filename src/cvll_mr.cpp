//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

arma::colvec vec(Rcpp::List a){
  int size = a.size();
  arma::colvec a1(size);
  for (int i = 0; i < size; i++) {
  a1[i] = a[i];
  }
  return a1;
}

Rcpp::List sexpl(arma::dmat &y){
  int size = y.size();
  Rcpp::List a1(size);
  for (int i = 0; i < size; i++) {
    a1[i] = y[i];
  }
  return a1;
}

// [[Rcpp::export]]
Rcpp::List cvll_mr(arma::dmat &x, arma::dmat &y, int n_row) {

  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  Rcpp::List mr;
  arma::colvec coef;
  arma::colvec resid;
  double b;
  Rcpp::List cvll_mr(n_row);

  Rcpp::Function rq = Rcpp::Environment("package:quantreg")["rq.fit"];

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i); // define obs i before change y
    rowyi = y.row(i);
    y.shed_row(i); // leaves out observation i
    xv = x.row(i); // define obs i before change x
    rowxi = x.row(i);
    x.shed_row(i); // leaves out observation i but changes x
    mr = rq(sexpl(x), sexpl(y));
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

// [[Rcpp::export]]
Rcpp::List cvll_rlm(arma::dmat &x, arma::colvec &y, int n_row, int n_col) {

  int n = n_row - 1;
  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  Rcpp::List rlm;
  arma::colvec coef;
  arma::colvec resid;
  double sig2;
  Rcpp::List cvll_rlm(n_row);

  Rcpp::Function rlm_fit = Rcpp::Environment("package:modeLLtest")["rlm_fit"];

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i); // define obs i before change y
    rowyi = y.row(i);
    y.shed_row(i); // leaves out observation i
    xv = x.row(i); // define obs i before change x
    rowxi = x.row(i);
    x.shed_row(i); // leaves out observation i but changes x
    rlm = rlm_fit(x, y);
    coef = vec(rlm("coefficients"));
    resid = vec(rlm("residuals")); // residuals
    sig2 = arma::as_scalar( arma::trans(resid) * resid /  (n - n_col) ); // SE of est
    cvll_rlm[i] = log(arma::normpdf(yv - xv * coef, 0, sig2));
    y.insert_rows(i, rowyi); // add y back in
    x.insert_rows(i, rowxi); // add x back in
  }

  return cvll_rlm;
}

