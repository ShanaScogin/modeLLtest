//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// function to change list into vector
arma::colvec vecmr(Rcpp::List a) {
  int size = a.size();
  arma::colvec a1(size);
  for (int i = 0; i < size; i++) {
    a1[i] = a[i];
  }
  return a1;
}

// function to run rq.fit from quantreg package
Rcpp::List rq(arma::dmat &x, arma::vec &y) {
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("quantreg");
  Rcpp::Function f = pkg["rq.fit"];
  return f(x, y);
}

// function to find the cvlls for median regression
// [[Rcpp::export]]
Rcpp::List cvll_mr(arma::dmat &x, arma::vec &y, int n_row, int n_col) {

  arma::rowvec rowyi;
  arma::rowvec rowxi;
  Rcpp::List mr;
  arma::colvec coef;
  arma::colvec resid;
  double b;
  Rcpp::List cvll_mr(n_row);

  // loop for leave-one-out cross-validation
  for (int i = 0; i < n_row; i++) {
    rowyi = y.row(i); // define obs i before change y
    y.shed_row(i); // leaves out observation i
    rowxi = x.row(i); // define obs i before change x
    x.shed_row(i); // leaves out observation i but changes x

    // wrap in if-else statement to check for singularity
    if(arma::rank(x) < n_col) {

      // cleaning up
      y.insert_rows(i, rowyi); // add y back in
      x.insert_rows(i, rowxi); // add x back in

      continue;

    } else {

      // model
      mr = rq(x, y);
      coef = vecmr(mr("coefficients"));
      resid = vecmr(mr("residuals")); // residuals
      b = arma::as_scalar( mean(abs(resid)) ); // dispersion param

      // output
      cvll_mr[i] = log( (1 / (2 * b) ) *
        exp( -abs( (rowyi - rowxi * coef) / b ) ) );

      // cleaning up
      y.insert_rows(i, rowyi); // add y back in
      x.insert_rows(i, rowxi); // add x back in

    }

  }

  return cvll_mr;
}
