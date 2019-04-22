// //[[Rcpp::depends(RcppArmadillo)]]
// #include <RcppArmadillo.h>
//
// arma::colvec vecmf(Rcpp::List a) {
//   int size = a.size();
//   arma::colvec a1(size);
//   for (int i = 0; i < size; i++) {
//     a1[i] = a[i];
//   }
//   return a1;
// }
//
// Rcpp::List coxph(arma::dmat &x, arma::vec &y) {
//   Rcpp::Environment pkg = Rcpp::Environment::namespace_env("survival");
//   Rcpp::Function f = pkg["coxph"];
//   return f(x, y);
// }
//
// Rcpp::List coxr(arma::dmat &x, arma::vec &y) {
//   Rcpp::Environment pkg = Rcpp::Environment::namespace_env("coxrobust");
//   Rcpp::Function f = pkg["coxr"];
//   return f(x, y);
// }
//
// // [[Rcpp::export]]
// Rcpp::List cvmf_loop(arma::dmat &x, arma::mat &y, arma::mat &w,
//                      int n_row, int n_col) {
//
//   arma::dmat yv;
//   arma::dmat xv;
//   arma::rowvec rowyi;
//   arma::rowvec rowxi;
//   Rcpp::List pesti;
//   Rcpp::List esti;
//   arma::colvec coef;
//   arma::colvec resid;
//   double b;
//   Rcpp::List cvll_mr(n_row);
//
//   for (int i = 0; i < n_row; i++) {
//     yv = y.row(i); // define obs i before change y
//     rowyi = y.row(i);
//     y.shed_row(i); // leaves out observation i
//     xv = x.row(i); // define obs i before change x
//     rowxi = x.row(i);
//     x.shed_row(i); // leaves out observation i but changes x
//     pesti = coxph(x, y);
//     coef = vecmr(mr("coefficients"));
//     resid = vecmr(mr("residuals")); // residuals
//     b = arma::as_scalar( mean(abs(resid)) ); // dispersion param
//     cvll_mr[i] = log( (1 / (2 * b) ) *
//       exp( -abs( (yv - xv * coef) / b ) ) );
//     y.insert_rows(i, rowyi); // add y back in
//     x.insert_rows(i, rowxi); // add x back in
//
// }

/***R
# Making empty vectors to prep for cross-validation
n <- nrow(x)
  cvll_r <- numeric(n)
  cvll_c <- numeric(n)

# Loop through for cross-validation
  for (i in 1:n){

# Remove current observation
    yi <- y[-i, ]
    xi <- as.matrix(x[-i, ])
    weightsi <- weights[-i]

# Estimate models without current observation i
    pesti <- survival::coxph(yi ~ xi,
                             method = method,
                             weights = weightsi, ##### need to test
                               na.action = na.action, ##### need to test
#excluding init since coxr only allows defaut
#excluding control
                                 singular.ok = singular.ok)
    esti <- coxrobust::coxr(yi ~ xi,
                            na.action = na.action, ### need to test this
                              trunc = trunc,
                                f.weight = f.weight, ### tested one round of this but more needed
                                  singular.ok = singular.ok)

# Check if any parameters were undefined without observation i
# This can happen with very sparse and/or very many covariates
# (e.g., fixed effect dummies)
    na_ind <- which(is.na(pesti$coefficients))

# Extract coefficients and covariates
    coef_p <- pesti$coefficients
    x_p <- x
    xi_p <- xi

# Remove any covariates with undefined effects
    if (length(na_ind) > 0){
      coef_p <- pesti$coefficients[-na_ind]
      x_p <- x[, -na_ind]
      xi_p <- xi[, -na_ind]
    }

# Compute the full and restricted partial likelihoods
    full_ll_r <- survival::coxph(y ~ offset(as.matrix(x) %*% cbind(esti$coefficients)),
                                 method = method)$loglik # this is unrestricted - fix this _r and _c
      full_ll_c <- survival::coxph(y ~ offset(as.matrix(x_p) %*% cbind(coef_p)),
                                   method = method)$loglik # this is unrestricted with nas dropped - fix this _r and _c
      esti_ll_r <- survival::coxph(yi ~ offset(as.matrix(xi) %*% cbind(esti$coefficients)),
                                   method = method)$loglik
      esti_ll_c <- survival::coxph(yi ~ offset(as.matrix(xi_p) %*% cbind(coef_p)),
                                   method = method)$loglik
### getting the likelihood - so just doesn't have to be irr
### offset() is forcing it to beta - linear predictor

# Store
      cvll_r[i] <- full_ll_r - esti_ll_r ## why are we leaving out observations???
      cvll_c[i] <- full_ll_c - esti_ll_c
  }

*/
