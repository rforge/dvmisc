#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int true_rangec_i(IntegerVector x) {
  int n = x.size();
  int currentx = x[0];
  int minx = currentx;
  int maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
    if (currentx > maxx) maxx = currentx;
  }
  int out = maxx - minx;
  return(out);
}