/**
 * Calculates the values of the leave-one-out cross-validated
 * log-likelihood function for OLS
 * @parma the matrix of independent variables
 * @parma the vector of dependent variables
 * @parma the number of observations
 *
 * @return void
 */

#include <Rcpp.h>
using namespace Rcpp ; // should this be #include "xxx.h"?

// [[Rcpp::exort}}]]
// the above will be taken out after troubleshooting

void cvll_ols(double *x, double *y, int *n_row) { //dmatrix for x?

  double cvll_ls ;
  double yt ;
  double xt ;
  double yv ;
  double xv ;
  double ls ;
  double sig ;

  for (int i = 0; i <* n_row; i++) { //check <* rather than <
    yt = y[-i]; // leaves out observation i
    xt = x[-i, ];
    yv = y[i];
    xv = x[i, ];
    ls = Rcpp::lm(yt ~ -1 + xt); // -1 takes out the intercept (1 is identifier)
    sig = R::summary(ls)$sigma; // dispersion parameter
    cvll_ls[i] = R::dnorm(yv - R::rbind(xv) %*% R::coef(ls),
               sd = sig, log = TRUE);
  }

  return list(LS = cvll_ls, // can one use list in cpp?
              n = length(y); // number of observations
}
