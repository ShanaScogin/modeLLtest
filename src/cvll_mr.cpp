//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

arma::colvec vecmr(Rcpp::List a) {
  int size = a.size();
  arma::colvec a1(size);
  for (int i = 0; i < size; i++) {
    a1[i] = a[i];
  }
  return a1;
}

Rcpp::List rq(arma::dmat &x, arma::vec &y) {
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("quantreg");
  Rcpp::Function f = pkg["rq.fit"];
  return f(x, y);
}

// [[Rcpp::export]]
Rcpp::List cvll_mr(arma::dmat &x, arma::vec &y, int n_row) {

  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  Rcpp::List mr;
  arma::colvec coef;
  arma::colvec resid;
  double b;
  Rcpp::List cvll_mr(n_row);

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i); // define obs i before change y
    rowyi = y.row(i);
    y.shed_row(i); // leaves out observation i
    xv = x.row(i); // define obs i before change x
    rowxi = x.row(i);
    x.shed_row(i); // leaves out observation i but changes x

    // model
    mr = rq(x, y);
    coef = vecmr(mr("coefficients"));
    resid = vecmr(mr("residuals")); // residuals
    b = arma::as_scalar( mean(abs(resid)) ); // dispersion param

    // output
    cvll_mr[i] = log( (1 / (2 * b) ) *
      exp( -abs( (yv - xv * coef) / b ) ) );

    // cleaning up
    y.insert_rows(i, rowyi); // add y back in
    x.insert_rows(i, rowxi); // add x back in
  }

  return cvll_mr;
}
