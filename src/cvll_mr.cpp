//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <math.h> // for log
#include <iostream> // for cout debugging
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
List cvll_mr(arma::dmat &x, arma::mat &y, int n_row, int n_col) {

  int n = n_row - 1;
  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  List mr;
  arma::dmat coef;
  arma::colvec resid;
  double b;
  List norm(n_row);
  List cvll_mr(n_row);

  Rcpp::Function rq = Environment("package:quantreg")["rq"];

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i); // define obs i before change y
    rowyi = y.row(i);
    y.shed_row(i); // leaves out observation i
    xv = x.row(i); // define obs i before change x
    rowxi = x.row(i);
    x.shed_row(i); // leaves out observation i but changes x
    mr = rq(x, y).;
//    coef = arma::solve(x, y); // fit model y ~ x
//    resid = y - x * coef; // residuals
    b = arma::as_scalar( arma::trans(resid)*resid/(n - n_col) ); // dispersion param
    cvll_mr[i] = log( (1 / (2 * b) ) *
      exp( -abs( (yv - xv * coef) / b ) ) );
    y.insert_rows(i, rowyi); // add y back in
    x.insert_rows(i, rowxi); // add x back in
  }

  return cvll_mr;
}

// [[Rcpp::export]]
List rq(){
  // calling rq()
  Rcpp::Function rq = Environment("package:quantreg")["rq.fit"];
}


/*** R
rcvll_mr <- function(x, y){ # cross-validated log likelihoods
  cvll_mr <- NA # empty vector for OLS cvlls
  for (i in 1:length(y)){
    yt <- y[-i] # leaves out observation i
    if (!is.matrix(try(solve(t(x) %*% x)))) { # returns TRUE if inv X'X does not exist
      next # skips to next iteration
    } else {
      xt <- x[-i, ] # if inverse X'X exists, creates object
    }
    yv <- y[i]
    xv <- x[i, ]
    mr <- quantreg::rq(yt ~ -1 + xt) # -1 takes out the intercept (1 is identifier)
      b <- mean(abs(residuals(mr))) # dispersion parameter
      cvll_mr[i] <- log(dlapl(yv - rbind(xv) %*% coef(mr), b = b))
  }
  return(list(cvll_mr)) # number of missing observations
}
dlapl <- function(a, b){
  return(1 / (2 * b) * exp(-abs(a / b)))
}
*/
