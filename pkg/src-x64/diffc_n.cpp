#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector diffc_n(NumericVector x, int lag = 1) {
  int n = x.size();
  int n_lesslag = n - lag;
  NumericVector out(n_lesslag);
  for (int a = 0; a < n_lesslag; ++a) {
    out[a] = x[a + lag] - x[a];
  }
  return(out);
}
