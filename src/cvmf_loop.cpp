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
// Rcpp::List coxr(arma::dmat &x, arma::vec &y) {
//   Rcpp::Environment pkg = Rcpp::Environment::namespace_env("coxrobust");
//   Rcpp::Function f = pkg["coxr"];
//   return f(x, y);
// }
//
// Rcpp::List ph(arma::dmat &x, arma::vec &y) {
//   Rcpp::Environment pkg = Rcpp::Environment::namespace_env("survival");
//   Rcpp::Function f = pkg["coxph"];
//   return f(x, y);
// }
//
// // [[Rcpp::export]]
// Rcpp::List cvll_mr(arma::dmat &x, arma::vec &y, int n_row) {
//
//   arma::vec yv;
//   arma::dmat xv;
//   arma::rowvec rowyi;
//   arma::rowvec rowxi;
//   Rcpp::List phout;
//   arma::colvec phcoef;
//   arma::colvec phresid;
//   double phb;
//   Rcpp::List rout;
//   arma::colvec rcoef;
//   arma::colvec rresid;
//   double rb;
//   Rcpp::List ph_ll_r;
//   Rcpp::List ph_ll_c;
//   Rcpp::List r_ll_r;
//   Rcpp::List r_ll_c;
//   Rcpp::List cvll_mf(n_row);
//
//   arma::dmat mod_one;
//   arma::dmat mod_two;
//   arma::dmat mod_three;
//   arma::dmat mod_four;
//
//   for (int i = 0; i < n_row; i++) {
//     yv = y; // define obj as y before change y
//     rowyi = y.row(i);
//     y.shed_row(i); // leaves out observation i
//     xv = x; // define obj before change x
//     rowxi = x.row(i);
//     x.shed_row(i); // leaves out observation i but changes x
//     phout = ph(x, y);
//     phcoef = vecmf(phout("coefficients"));
// //    phresid = vecmf(phout("residuals")); // residuals
// //    phb = arma::as_scalar( mean(abs(phresid)) ); // dispersion param
//
//     rout = coxr(x, y);
//     rcoef = vecmf(rout("coefficients"));
// //    rresid = vecmf(rout("residuals")); // residuals
// //    rb = arma::as_scalar( mean(abs(rresid)) ); // dispersion param
//
//     mod_one  = xv * rcoef;
//     mod_two = xv * phcoef; // need to take out the undefined effects
//     mod_three = x * rcoef;
//     mod_four = x * phcoef; // need to take out undefined effects
//
//     ph_ll_r = ph(mod_one, yv)("loglik");
//     ph_ll_c = ph(mod_two, yv)("loglik");
//     r_ll_r = ph(mod_three, y)("loglik");
//     r_ll_c = ph(mod_four, y)("loglik");
//
// //    cvll_mf[i] = log( (1 / (2 * phb) ) *
// //      exp( -abs( (yv - xv * phcoef) / phb ) ) ); //make this a matrix? or add another list?
//     y.insert_rows(i, rowyi); // add y back in
//     x.insert_rows(i, rowxi); // add x back in
//   }
//
//   return ph_ll_r;
// }

/***R
# Making empty vectors to prep for cross-validation
# n <- nrow(x)
#   cvll_r <- numeric(n)
#   cvll_c <- numeric(n)

# # Loop through for cross-validation
#   for (i in 1:n){
#
# # Remove current observation
#     yi <- y[-i, ]
#     xi <- as.matrix(x[-i, ])
#     weightsi <- weights[-i]
#
# # Estimate models without current observation i
#     pesti <- survival::coxph(yi ~ xi,
#                              method = method,
#                              weights = weightsi, ##### need to test
#                                na.action = na.action, ##### need to test
# #excluding init since coxr only allows defaut
# #excluding control
#                                  singular.ok = singular.ok)
#     esti <- coxrobust::coxr(yi ~ xi,
#                             na.action = na.action, ### need to test this
#                               trunc = trunc,
#                                 f.weight = f.weight, ### tested one round of this but more needed
#                                   singular.ok = singular.ok)
#
# # Check if any parameters were undefined without observation i
# # This can happen with very sparse and/or very many covariates
# # (e.g., fixed effect dummies)
#     na_ind <- which(is.na(pesti$coefficients))
#
# # Extract coefficients and covariates
#     coef_p <- pesti$coefficients
#     x_p <- x
#     xi_p <- xi
#
# # Remove any covariates with undefined effects
#     if (length(na_ind) > 0){
#       coef_p <- pesti$coefficients[-na_ind]
#       x_p <- x[, -na_ind]
#       xi_p <- xi[, -na_ind]
#     }
#
# # Compute the full and restricted partial likelihoods
#     full_ll_r <- survival::coxph(y ~ offset(as.matrix(x) %*% cbind(esti$coefficients)),
#                                  method = method)$loglik # this is unrestricted - fix this _r and _c
#       full_ll_c <- survival::coxph(y ~ offset(as.matrix(x_p) %*% cbind(coef_p)),
#                                    method = method)$loglik # this is unrestricted with nas dropped - fix this _r and _c
#       esti_ll_r <- survival::coxph(yi ~ offset(as.matrix(xi) %*% cbind(esti$coefficients)),
#                                    method = method)$loglik
#       esti_ll_c <- survival::coxph(yi ~ offset(as.matrix(xi_p) %*% cbind(coef_p)),
#                                    method = method)$loglik
# ### getting the likelihood - so just doesn't have to be irr
# ### offset() is forcing it to beta - linear predictor
#
# # Store
#       cvll_r[i] <- full_ll_r - esti_ll_r ## why are we leaving out observations???
#       cvll_c[i] <- full_ll_c - esti_ll_c
#   }

*/
