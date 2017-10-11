#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double true_rangec_n(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double minx = currentx;
  double maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx < minx) minx = currentx;
    if (currentx > maxx) maxx = currentx;
  }
  double out = maxx - minx;
  return(out);
}