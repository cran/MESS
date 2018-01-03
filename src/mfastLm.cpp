// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp;

//' Fast marginal simple regresion analyses
//' 
//' Fast computation of simple regression slopes for each predictor represented by a column in a matrix
//'
//' Missing values (NA, Inf, NaN) are completely disregarded and pairwise complete cases are used for the analysis.
//' 
//' @param y A vector of outcomes.
//' @param x A matrix of regressor variables. Must have the same number of rows as the length of y. 
//' @param addintercept A logical that determines if the intercept should be included in all analyses (TRUE) or not (FALSE)
//' @return A data frame with three variables: coefficients, stderr, and tstat that gives the slope estimate, the corresponding standard error, and their ratio for each column in x.
//' @author Claus Ekstrom <claus@@rprimer.dk>
//' @examples
//' \dontrun{
//'   // Generate 100000 predictors and 100 observations
//'   x <- matrix(rnorm(100*100000), nrow=100)
//'   y <- rnorm(100, mean=x[,1])
//'   mfastLmCpp(y, x)
//'
//' }
//' @export
// [[Rcpp::export]]
DataFrame mfastLmCpp(NumericVector y, NumericMatrix x, bool addintercept=true) {
  arma::uword n = x.nrow(), k = x.ncol();
  int df = n-1;

  // Sanity checks
  if (y.size() != n) {
    stop("The length of y and the number of rows in x must match");
  }
  
  arma::mat X(x.begin(), n, k, false);
  arma::colvec Y(y.begin(), y.size(), false);
  arma::mat newX;
  arma::mat x0=arma::ones<arma::mat>(n,1);

  arma::colvec rescoef = arma::zeros(k), resse = arma::zeros(k), tstat = arma::zeros(k);
  arma::colvec coef, resid, stderrest;
  double sig2;

  arma::colvec indY = arma::zeros(n), indX = arma::zeros(n);
  for (arma::uword j=0; j<n; j++) {
    indY(j) = R_finite(Y(j));
  }
  
  for (arma::uword i=0; i<k; i++) {
    if (addintercept) {
      newX = join_rows(X.col(i), x0);
    } else {
      newX = X.col(i);
    }
    // Handling missings
    for (arma::uword j=0; j<n; j++) {
      indX(j) = R_finite(X(j, i));
    }

    arma::uvec index = arma::conv_to<arma::uvec>::from(find(indY % indX));

    df = index.n_elem-1;
    if (addintercept) {
      df -= 1;
    }



    coef = arma::solve(newX.rows(index), Y(index));
    rescoef(i) = coef(0);
    resid = Y(index) - newX.rows(index)*coef;
    sig2 = arma::as_scalar(arma::trans(resid)*resid/df);
    stderrest = arma::sqrt(sig2 * arma::diagvec( arma::inv(arma::trans(newX.rows(index))*newX.rows(index))) );
    resse(i) = stderrest(0);
    tstat(i) = rescoef(i)/resse(i);
  }

  // create a new data frame and return it
  return DataFrame::create(Rcpp::Named("coefficients")=rescoef,
			   Rcpp::Named("stderr")=resse,
			   Rcpp::Named("tstat")=tstat
			   );
}
