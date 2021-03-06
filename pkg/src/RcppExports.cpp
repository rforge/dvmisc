// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

// cov_i
double cov_i(IntegerVector x, IntegerVector y);
RcppExport SEXP _dvmisc_cov_i(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(cov_i(x, y));
    return rcpp_result_gen;
END_RCPP
}
// cov_n
double cov_n(NumericVector x, NumericVector y);
RcppExport SEXP _dvmisc_cov_n(SEXP xSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(cov_n(x, y));
    return rcpp_result_gen;
END_RCPP
}
// diff_i
IntegerVector diff_i(IntegerVector x, int lag);
RcppExport SEXP _dvmisc_diff_i(SEXP xSEXP, SEXP lagSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type lag(lagSEXP);
    rcpp_result_gen = Rcpp::wrap(diff_i(x, lag));
    return rcpp_result_gen;
END_RCPP
}
// diff_n
NumericVector diff_n(NumericVector x, int lag);
RcppExport SEXP _dvmisc_diff_n(SEXP xSEXP, SEXP lagSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< int >::type lag(lagSEXP);
    rcpp_result_gen = Rcpp::wrap(diff_n(x, lag));
    return rcpp_result_gen;
END_RCPP
}
// diff1_i
IntegerVector diff1_i(IntegerVector x);
RcppExport SEXP _dvmisc_diff1_i(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(diff1_i(x));
    return rcpp_result_gen;
END_RCPP
}
// diff1_n
NumericVector diff1_n(NumericVector x);
RcppExport SEXP _dvmisc_diff1_n(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(diff1_n(x));
    return rcpp_result_gen;
END_RCPP
}
// max_n
double max_n(NumericVector x);
RcppExport SEXP _dvmisc_max_n(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(max_n(x));
    return rcpp_result_gen;
END_RCPP
}
// mean_i
double mean_i(IntegerVector x);
RcppExport SEXP _dvmisc_mean_i(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(mean_i(x));
    return rcpp_result_gen;
END_RCPP
}
// min_n
double min_n(NumericVector x);
RcppExport SEXP _dvmisc_min_n(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(min_n(x));
    return rcpp_result_gen;
END_RCPP
}
// range_i
IntegerVector range_i(IntegerVector x);
RcppExport SEXP _dvmisc_range_i(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(range_i(x));
    return rcpp_result_gen;
END_RCPP
}
// range_n
NumericVector range_n(NumericVector x);
RcppExport SEXP _dvmisc_range_n(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(range_n(x));
    return rcpp_result_gen;
END_RCPP
}
// sum_i
int sum_i(IntegerVector x);
RcppExport SEXP _dvmisc_sum_i(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(sum_i(x));
    return rcpp_result_gen;
END_RCPP
}
// true_range_i
int true_range_i(IntegerVector x);
RcppExport SEXP _dvmisc_true_range_i(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(true_range_i(x));
    return rcpp_result_gen;
END_RCPP
}
// true_range_n
double true_range_n(NumericVector x);
RcppExport SEXP _dvmisc_true_range_n(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(true_range_n(x));
    return rcpp_result_gen;
END_RCPP
}
// var_i
double var_i(IntegerVector x);
RcppExport SEXP _dvmisc_var_i(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(var_i(x));
    return rcpp_result_gen;
END_RCPP
}
// var_n
double var_n(NumericVector x);
RcppExport SEXP _dvmisc_var_n(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(var_n(x));
    return rcpp_result_gen;
END_RCPP
}
// weighted_mean_ii
double weighted_mean_ii(IntegerVector x, IntegerVector w);
RcppExport SEXP _dvmisc_weighted_mean_ii(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_mean_ii(x, w));
    return rcpp_result_gen;
END_RCPP
}
// weighted_mean_in
double weighted_mean_in(IntegerVector x, NumericVector w);
RcppExport SEXP _dvmisc_weighted_mean_in(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_mean_in(x, w));
    return rcpp_result_gen;
END_RCPP
}
// weighted_mean_ni
double weighted_mean_ni(NumericVector x, IntegerVector w);
RcppExport SEXP _dvmisc_weighted_mean_ni(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_mean_ni(x, w));
    return rcpp_result_gen;
END_RCPP
}
// weighted_mean_nn
double weighted_mean_nn(NumericVector x, NumericVector w);
RcppExport SEXP _dvmisc_weighted_mean_nn(SEXP xSEXP, SEXP wSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type w(wSEXP);
    rcpp_result_gen = Rcpp::wrap(weighted_mean_nn(x, w));
    return rcpp_result_gen;
END_RCPP
}
// which_max_im
IntegerVector which_max_im(IntegerMatrix x);
RcppExport SEXP _dvmisc_which_max_im(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_max_im(x));
    return rcpp_result_gen;
END_RCPP
}
// which_max_iv
int which_max_iv(IntegerVector x);
RcppExport SEXP _dvmisc_which_max_iv(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_max_iv(x));
    return rcpp_result_gen;
END_RCPP
}
// which_max_nm
IntegerVector which_max_nm(NumericMatrix x);
RcppExport SEXP _dvmisc_which_max_nm(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_max_nm(x));
    return rcpp_result_gen;
END_RCPP
}
// which_max_nv
int which_max_nv(NumericVector x);
RcppExport SEXP _dvmisc_which_max_nv(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_max_nv(x));
    return rcpp_result_gen;
END_RCPP
}
// which_min_im
IntegerVector which_min_im(IntegerMatrix x);
RcppExport SEXP _dvmisc_which_min_im(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_min_im(x));
    return rcpp_result_gen;
END_RCPP
}
// which_min_iv
int which_min_iv(IntegerVector x);
RcppExport SEXP _dvmisc_which_min_iv(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< IntegerVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_min_iv(x));
    return rcpp_result_gen;
END_RCPP
}
// which_min_nm
IntegerVector which_min_nm(NumericMatrix x);
RcppExport SEXP _dvmisc_which_min_nm(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_min_nm(x));
    return rcpp_result_gen;
END_RCPP
}
// which_min_nv
int which_min_nv(NumericVector x);
RcppExport SEXP _dvmisc_which_min_nv(SEXP xSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type x(xSEXP);
    rcpp_result_gen = Rcpp::wrap(which_min_nv(x));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dvmisc_cov_i", (DL_FUNC) &_dvmisc_cov_i, 2},
    {"_dvmisc_cov_n", (DL_FUNC) &_dvmisc_cov_n, 2},
    {"_dvmisc_diff_i", (DL_FUNC) &_dvmisc_diff_i, 2},
    {"_dvmisc_diff_n", (DL_FUNC) &_dvmisc_diff_n, 2},
    {"_dvmisc_diff1_i", (DL_FUNC) &_dvmisc_diff1_i, 1},
    {"_dvmisc_diff1_n", (DL_FUNC) &_dvmisc_diff1_n, 1},
    {"_dvmisc_max_n", (DL_FUNC) &_dvmisc_max_n, 1},
    {"_dvmisc_mean_i", (DL_FUNC) &_dvmisc_mean_i, 1},
    {"_dvmisc_min_n", (DL_FUNC) &_dvmisc_min_n, 1},
    {"_dvmisc_range_i", (DL_FUNC) &_dvmisc_range_i, 1},
    {"_dvmisc_range_n", (DL_FUNC) &_dvmisc_range_n, 1},
    {"_dvmisc_sum_i", (DL_FUNC) &_dvmisc_sum_i, 1},
    {"_dvmisc_true_range_i", (DL_FUNC) &_dvmisc_true_range_i, 1},
    {"_dvmisc_true_range_n", (DL_FUNC) &_dvmisc_true_range_n, 1},
    {"_dvmisc_var_i", (DL_FUNC) &_dvmisc_var_i, 1},
    {"_dvmisc_var_n", (DL_FUNC) &_dvmisc_var_n, 1},
    {"_dvmisc_weighted_mean_ii", (DL_FUNC) &_dvmisc_weighted_mean_ii, 2},
    {"_dvmisc_weighted_mean_in", (DL_FUNC) &_dvmisc_weighted_mean_in, 2},
    {"_dvmisc_weighted_mean_ni", (DL_FUNC) &_dvmisc_weighted_mean_ni, 2},
    {"_dvmisc_weighted_mean_nn", (DL_FUNC) &_dvmisc_weighted_mean_nn, 2},
    {"_dvmisc_which_max_im", (DL_FUNC) &_dvmisc_which_max_im, 1},
    {"_dvmisc_which_max_iv", (DL_FUNC) &_dvmisc_which_max_iv, 1},
    {"_dvmisc_which_max_nm", (DL_FUNC) &_dvmisc_which_max_nm, 1},
    {"_dvmisc_which_max_nv", (DL_FUNC) &_dvmisc_which_max_nv, 1},
    {"_dvmisc_which_min_im", (DL_FUNC) &_dvmisc_which_min_im, 1},
    {"_dvmisc_which_min_iv", (DL_FUNC) &_dvmisc_which_min_iv, 1},
    {"_dvmisc_which_min_nm", (DL_FUNC) &_dvmisc_which_min_nm, 1},
    {"_dvmisc_which_min_nv", (DL_FUNC) &_dvmisc_which_min_nv, 1},
    {NULL, NULL, 0}
};

RcppExport void R_init_dvmisc(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
