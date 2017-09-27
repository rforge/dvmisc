#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double minc_n(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double minx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
  }
  return(minx);
}
