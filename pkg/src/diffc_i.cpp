#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector diffc_i(IntegerVector x, int lag = 1) {
  int n = x.size();
  int n_lesslag = n - lag;
  IntegerVector out(n_lesslag);
  for (int a = 0; a < n_lesslag; ++a) {
    out[a] = x[a + lag] - x[a];
  }
  return(out);
}