#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double weighted_meanc_ii(IntegerVector x, IntegerVector w) {
  int n = x.size();
  double sumx = 0;
  double sumw = 0;
  double weight;
  for (int a = 0; a < n; ++a) {
    weight = w[a];
    sumx += x[a] * weight;
    sumw += weight;
  }
  double meanx = sumx / sumw;
  return(meanx);
}
