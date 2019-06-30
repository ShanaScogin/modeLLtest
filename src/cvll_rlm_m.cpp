//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>

// function to change list into vector
arma::colvec vecrobustm(Rcpp::List a) {
  int size = a.size();
  arma::colvec a1(size);
  for (int i = 0; i < size; i++) {
    a1[i] = a[i];
  }
  return a1;
}

// function to use rlm from MASS
// future developments will put this into rcpp to decrease runtime
Rcpp::List robustm(arma::dmat &x, arma::vec &y) {
  Rcpp::Environment pkg = Rcpp::Environment::namespace_env("MASS");
  Rcpp::Function f = pkg["rlm"];
  return f(x, y, Rcpp::Named("maxit") = 100); // default is 20 in MASS::rlm
}

// function to use gamma() function from base R
double gamm(double &x) {
  Rcpp::Environment pkg = Rcpp::Environment::base_env();
  Rcpp::Function f = pkg["gamma"];
  return Rcpp::as<double>(f(x));
}

// function to find the cvlls for m method of estimating robust regression
// [[Rcpp::export]]
Rcpp::List cvll_rlm_m(arma::dmat &x, arma::colvec &y, int n_row, int n_col) {

  arma::rowvec rowyi;
  arma::rowvec rowxi;
  Rcpp::List rlm;
  arma::colvec coef;
  arma::colvec input;
  double sig;
  Rcpp::List cvll_rlm(n_row);

  // variables for t density function
  double gam_a;
  double gamm_a;
  double gam_b;
  double gamm_b;
  double dst_a;
  Rcpp::NumericVector dst_b;
  double dst_c;
  Rcpp::NumericVector dst_d;
  arma::colvec dst_e;
  double dst_f;
  double dst_g;

  // set up for t density function
  int df = 4; // hard coding df to be 4
              // 4 - 6 is usually used in RR
  gam_a = (df + 1.0) / 2.0;
  gamm_a = gamm(gam_a);
  gam_b = df / 2.0;
  gamm_b = gamm(gam_b);

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
      rlm = robustm(x, y);
      coef = vecrobustm(rlm("coefficients"));
      input = rowyi - rowxi * coef;
      sig = arma::as_scalar(vecrobustm(rlm("s"))); // scale param

      // t density function - PDF for RR
      dst_a = sig * std::sqrt(df * M_PI);
      dst_b = pow( input, 2 );
      dst_c = pow( sig, 2 );
      dst_d = df + dst_b / dst_c;
      dst_e = pow( dst_d / df, - (df + 1) / 2 );
      dst_f =  dst_a * gamm_b;
      dst_g = gamm_a / dst_f;
      cvll_rlm[i] = log(dst_g * dst_e);

      // cleaning up
      y.insert_rows(i, rowyi); // add y back in
      x.insert_rows(i, rowxi); // add x back in

    }

  }

  return cvll_rlm;
}
