#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
IntegerVector which_minc_im(IntegerMatrix x) {
  int ncols = x.ncol();
  int nrows = x.nrow();
  int currentx = x(0, 0);
  int minx = currentx;
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