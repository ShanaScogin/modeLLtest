//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// [[Rcpp::export]]
Rcpp::List cvll_ols(arma::dmat &x, arma::vec &y, int n_row, int n_col) {

  int n = n_row - 1;
  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  arma::dmat coef;
  arma::colvec resid;
  double sig;
  Rcpp::List cvll_ls(n_row);

  // leave one out cross validation loop
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
      coef = arma::solve(x, y); // fit model y ~ x
      resid = y - x * coef; // residuals
      sig = arma::as_scalar(sqrt( arma::trans(resid) * resid /  (n - n_col) )); // sqrt(SE of est)

      // output
      cvll_ls[i] = log(arma::normpdf(rowyi - rowxi * coef, 0, sig));

      // cleaning up
      y.insert_rows(i, rowyi); // add y back in
      x.insert_rows(i, rowxi); // add x back in

    }

  }

  return cvll_ls;
}
