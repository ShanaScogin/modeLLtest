//[[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
#include <iostream> // for cout debugging
using namespace Rcpp;
using namespace std;

// [[Rcpp::export]]
arma::vec cvll_ols(arma::dmat x, arma::mat y, int n_row, int n_col) {

  int n = n_row - 1;
  arma::dmat yv;
  arma::dmat xv;
  arma::rowvec rowyi;
  arma::rowvec rowxi;
  arma::dmat coef;
  arma::colvec resid;
  double sig2;
  arma::vec norm;
  arma::rowvec cvll_ls(n_row);

  for (int i = 0; i < n_row; i++) {
    yv = y.row(i); // define obs i before change y
    rowyi = y.row(i);
    y.shed_row(i); // leaves out observation i
    xv = x.row(i); // define obs i before change x
    rowxi = x.row(i);
    x.shed_row(i); // leaves out observation i but changes x
//    cout << x << endl ;
    coef = arma::solve(x, y); // fit model y ~ X
    resid = y - x * coef; // residuals
    coef.shed_row(0); // remove intercept
    sig2 = arma::as_scalar( arma::trans(resid)*resid/(n - n_col) ); // SE of est
    // ?? does this k include the ones????
    norm = xv * coef;
//    cvll_ls[i] = Rcpp::sugar::dnorm(yv - xv * coef, sig2) ;
//    cvll_ls[i] =  (1 / (2 * sig2)); // * exp(-abs((yv - xv * coef) / sig2)));
    y.insert_rows(i, rowyi); // add y back in
    x.insert_rows(i, rowxi); // add x back in
  }

  return norm ;
}

/*** R
rcvll_ols <- function(x, y){ # cross-validated log likelihoods
  cvll_ls <- NA # empty vector for OLS cvlls
  for (i in 1:length(y)){
    yt <- y[-i] # leaves out observation i
    xt <- x[-i, ]
    yv <- y[i]
    xv <- x[i, ]
    ls <- lm(yt ~ -1 + xt) # -1 takes out the intercept (1 is identifier)
      sig <- summary(ls)$sigma # dispersion parameter
      cvll_ls[i] <- dnorm(yv - rbind(xv) %*% coef(ls), sd = sig, log = TRUE)
      norm <- yv - rbind(xv) %*% coef(ls)
      normsm <- rbind(xv) %*% coef(ls)
      normsmb <- xv %*% coef(ls)
  }

  return(list(LS = cvll_ls,
              n = length(y), # number of observations
              sig = sig,
              norm = norm,
              normsm = normsm,
              normsmb = normsmb)) # number of missing observations
}
*/

