#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector diff1c_i(IntegerVector x) {
  int n = x.size();
  int n_less1 = n - 1;
  IntegerVector out(n_less1);
  for (int a = 0; a < n_less1; ++a) {
    out[a] = x[a + 1] - x[a];
  }
  return(out);
}