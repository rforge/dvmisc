#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int which_minc_nv(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double minx = currentx;
  int loc = 0;
  for (int a = 0; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) {
      minx = currentx;
      loc = a;
    }
  }
  loc += 1;
  return(loc);
}