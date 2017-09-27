#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int maxc_i(IntegerVector x) {
  int n = x.size();
  int currentx = x[0];
  int maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx > maxx) maxx = currentx;
  }
  return(maxx);
}
