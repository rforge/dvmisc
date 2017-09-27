#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double varc_i(IntegerVector x) {
  double n = x.size();
  double nless1 = n - 1;
  double sumx = 0;
  double sumx2 = 0;
  for (int a = 0; a < n; ++a) {
    sumx += x[a];
    sumx2 += pow(x[a], 2);
  }
  double s2 = 1 / nless1 * (sumx2 - n * pow(sumx / n, 2));
  return(s2);
}
