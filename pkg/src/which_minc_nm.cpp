#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector which_minc_nm(NumericMatrix x) {
  int ncols = x.ncol();
  int nrows = x.nrow();
  double currentx = x(0, 0);
  double minx = currentx;
  IntegerVector loc(2);
  loc[0] = 0;
  loc[1] = 0;
  for (int a = 0; a < ncols; ++a) {
    for (int b = 0; b < nrows; ++b) {
      currentx = x(b, a);
      if (currentx < minx) {
        minx = currentx;
        loc[0] = b;
        loc[1] = a;
      }
    }
  }
  loc[0] += 1;
  loc[1] += 1;
  return(loc);
}