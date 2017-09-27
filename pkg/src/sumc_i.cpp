#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
int sumc_i(IntegerVector x) {
  int n = x.size();
  int sumx = 0;
  for (int a = 0; a < n; ++a) {
    sumx += x[a];
  }
  return(sumx);
}
