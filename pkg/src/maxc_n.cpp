#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
double maxc_n(NumericVector x) {
  int n = x.size();
  double currentx = x[0];
  double maxx = currentx;
  for (int a = 1; a < n; ++a) {
    currentx = x[a];
    if (currentx > maxx) maxx = currentx;
  }
  return(maxx);
}
