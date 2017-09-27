#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int minc_i(IntegerVector x) {
  int n = x.size();
  int currentx = x[0];
  int minx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
  }
  return(minx);
}
