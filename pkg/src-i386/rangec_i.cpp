#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector rangec_i(IntegerVector x) {
  int n = x.size();
  int currentx = x[0];
  int minx = currentx;
  int maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
    if (currentx > maxx) maxx = currentx;
  }
  IntegerVector out(2);
  out[0] = minx;
  out[1] = maxx;
  return(out);
}