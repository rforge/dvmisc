#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double meanc_i(IntegerVector x) {
  int n = x.size();
  double sumx = 0;
  for (int a = 0; a < n; ++a) {
    sumx += x[a];
  }
  double meanx = sumx / n;
  return(meanx);
}
