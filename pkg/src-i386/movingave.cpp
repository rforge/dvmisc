#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
NumericVector movingave(NumericVector x, int window) {
  int n = x.size();
  NumericVector out(n - window + 1);
  double sum = 0;
  int a;
  for (a = 0; a < window; ++a) {
    sum += x[a];
  }
  out[0] = sum / window;
  int index = 0;
  for (a = window; a < n; ++a) {
    sum = sum + x[a] - x[a - window];
    index += 1;
    out[index] = sum / window;
  }
  return(out);
}