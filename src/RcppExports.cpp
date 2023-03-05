// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <RcppArmadillo.h>
#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// BranchAndBoundCpp
List BranchAndBoundCpp(NumericMatrix x, NumericVector y, NumericVector offset, IntegerVector indices, IntegerVector num, IntegerMatrix interactions, std::string method, int m, std::string Link, std::string Dist, unsigned int nthreads, double tol, int maxit, IntegerVector keep, int maxsize, std::string metric, bool display_progress, unsigned int NumBest, double cutoff);
RcppExport SEXP _BranchGLM_BranchAndBoundCpp(SEXP xSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP indicesSEXP, SEXP numSEXP, SEXP interactionsSEXP, SEXP methodSEXP, SEXP mSEXP, SEXP LinkSEXP, SEXP DistSEXP, SEXP nthreadsSEXP, SEXP tolSEXP, SEXP maxitSEXP, SEXP keepSEXP, SEXP maxsizeSEXP, SEXP metricSEXP, SEXP display_progressSEXP, SEXP NumBestSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type num(numSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type interactions(interactionsSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type Link(LinkSEXP);
    Rcpp::traits::input_parameter< std::string >::type Dist(DistSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nthreads(nthreadsSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type keep(keepSEXP);
    Rcpp::traits::input_parameter< int >::type maxsize(maxsizeSEXP);
    Rcpp::traits::input_parameter< std::string >::type metric(metricSEXP);
    Rcpp::traits::input_parameter< bool >::type display_progress(display_progressSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type NumBest(NumBestSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(BranchAndBoundCpp(x, y, offset, indices, num, interactions, method, m, Link, Dist, nthreads, tol, maxit, keep, maxsize, metric, display_progress, NumBest, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// BackwardBranchAndBoundCpp
List BackwardBranchAndBoundCpp(NumericMatrix x, NumericVector y, NumericVector offset, IntegerVector indices, IntegerVector num, IntegerMatrix interactions, std::string method, int m, std::string Link, std::string Dist, unsigned int nthreads, double tol, int maxit, IntegerVector keep, std::string metric, bool display_progress, unsigned int NumBest, double cutoff);
RcppExport SEXP _BranchGLM_BackwardBranchAndBoundCpp(SEXP xSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP indicesSEXP, SEXP numSEXP, SEXP interactionsSEXP, SEXP methodSEXP, SEXP mSEXP, SEXP LinkSEXP, SEXP DistSEXP, SEXP nthreadsSEXP, SEXP tolSEXP, SEXP maxitSEXP, SEXP keepSEXP, SEXP metricSEXP, SEXP display_progressSEXP, SEXP NumBestSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type num(numSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type interactions(interactionsSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type Link(LinkSEXP);
    Rcpp::traits::input_parameter< std::string >::type Dist(DistSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nthreads(nthreadsSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type keep(keepSEXP);
    Rcpp::traits::input_parameter< std::string >::type metric(metricSEXP);
    Rcpp::traits::input_parameter< bool >::type display_progress(display_progressSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type NumBest(NumBestSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(BackwardBranchAndBoundCpp(x, y, offset, indices, num, interactions, method, m, Link, Dist, nthreads, tol, maxit, keep, metric, display_progress, NumBest, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// SwitchBranchAndBoundCpp
List SwitchBranchAndBoundCpp(NumericMatrix x, NumericVector y, NumericVector offset, IntegerVector indices, IntegerVector num, IntegerMatrix interactions, std::string method, int m, std::string Link, std::string Dist, unsigned int nthreads, double tol, int maxit, IntegerVector keep, std::string metric, bool display_progress, unsigned int NumBest, double cutoff);
RcppExport SEXP _BranchGLM_SwitchBranchAndBoundCpp(SEXP xSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP indicesSEXP, SEXP numSEXP, SEXP interactionsSEXP, SEXP methodSEXP, SEXP mSEXP, SEXP LinkSEXP, SEXP DistSEXP, SEXP nthreadsSEXP, SEXP tolSEXP, SEXP maxitSEXP, SEXP keepSEXP, SEXP metricSEXP, SEXP display_progressSEXP, SEXP NumBestSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type num(numSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type interactions(interactionsSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type Link(LinkSEXP);
    Rcpp::traits::input_parameter< std::string >::type Dist(DistSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nthreads(nthreadsSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type keep(keepSEXP);
    Rcpp::traits::input_parameter< std::string >::type metric(metricSEXP);
    Rcpp::traits::input_parameter< bool >::type display_progress(display_progressSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type NumBest(NumBestSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(SwitchBranchAndBoundCpp(x, y, offset, indices, num, interactions, method, m, Link, Dist, nthreads, tol, maxit, keep, metric, display_progress, NumBest, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// BranchGLMfit
List BranchGLMfit(NumericMatrix x, NumericVector y, NumericVector offset, NumericVector init, std::string method, unsigned int m, std::string Link, std::string Dist, unsigned int nthreads, double tol, int maxit, bool GetInit);
RcppExport SEXP _BranchGLM_BranchGLMfit(SEXP xSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP initSEXP, SEXP methodSEXP, SEXP mSEXP, SEXP LinkSEXP, SEXP DistSEXP, SEXP nthreadsSEXP, SEXP tolSEXP, SEXP maxitSEXP, SEXP GetInitSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type init(initSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type Link(LinkSEXP);
    Rcpp::traits::input_parameter< std::string >::type Dist(DistSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nthreads(nthreadsSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< bool >::type GetInit(GetInitSEXP);
    rcpp_result_gen = Rcpp::wrap(BranchGLMfit(x, y, offset, init, method, m, Link, Dist, nthreads, tol, maxit, GetInit));
    return rcpp_result_gen;
END_RCPP
}
// MetricIntervalCpp
List MetricIntervalCpp(NumericMatrix x, NumericVector y, NumericVector offset, IntegerVector indices, IntegerVector num, IntegerMatrix models, std::string method, int m, std::string Link, std::string Dist, unsigned int nthreads, double tol, int maxit, std::string metric, NumericVector mle, NumericVector se, NumericVector best, double goal, std::string rootMethod);
RcppExport SEXP _BranchGLM_MetricIntervalCpp(SEXP xSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP indicesSEXP, SEXP numSEXP, SEXP modelsSEXP, SEXP methodSEXP, SEXP mSEXP, SEXP LinkSEXP, SEXP DistSEXP, SEXP nthreadsSEXP, SEXP tolSEXP, SEXP maxitSEXP, SEXP metricSEXP, SEXP mleSEXP, SEXP seSEXP, SEXP bestSEXP, SEXP goalSEXP, SEXP rootMethodSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type num(numSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type models(modelsSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type Link(LinkSEXP);
    Rcpp::traits::input_parameter< std::string >::type Dist(DistSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nthreads(nthreadsSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< std::string >::type metric(metricSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type mle(mleSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type se(seSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type best(bestSEXP);
    Rcpp::traits::input_parameter< double >::type goal(goalSEXP);
    Rcpp::traits::input_parameter< std::string >::type rootMethod(rootMethodSEXP);
    rcpp_result_gen = Rcpp::wrap(MetricIntervalCpp(x, y, offset, indices, num, models, method, m, Link, Dist, nthreads, tol, maxit, metric, mle, se, best, goal, rootMethod));
    return rcpp_result_gen;
END_RCPP
}
// ForwardCpp
List ForwardCpp(NumericMatrix x, NumericVector y, NumericVector offset, IntegerVector indices, IntegerVector num, IntegerMatrix interactions, std::string method, int m, std::string Link, std::string Dist, unsigned int nthreads, double tol, int maxit, IntegerVector keep, unsigned int steps, std::string metric);
RcppExport SEXP _BranchGLM_ForwardCpp(SEXP xSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP indicesSEXP, SEXP numSEXP, SEXP interactionsSEXP, SEXP methodSEXP, SEXP mSEXP, SEXP LinkSEXP, SEXP DistSEXP, SEXP nthreadsSEXP, SEXP tolSEXP, SEXP maxitSEXP, SEXP keepSEXP, SEXP stepsSEXP, SEXP metricSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type num(numSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type interactions(interactionsSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type Link(LinkSEXP);
    Rcpp::traits::input_parameter< std::string >::type Dist(DistSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nthreads(nthreadsSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type keep(keepSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type steps(stepsSEXP);
    Rcpp::traits::input_parameter< std::string >::type metric(metricSEXP);
    rcpp_result_gen = Rcpp::wrap(ForwardCpp(x, y, offset, indices, num, interactions, method, m, Link, Dist, nthreads, tol, maxit, keep, steps, metric));
    return rcpp_result_gen;
END_RCPP
}
// BackwardCpp
List BackwardCpp(NumericMatrix x, NumericVector y, NumericVector offset, IntegerVector indices, IntegerVector num, IntegerMatrix interactions, std::string method, int m, std::string Link, std::string Dist, unsigned int nthreads, double tol, int maxit, IntegerVector keep, unsigned int steps, std::string metric);
RcppExport SEXP _BranchGLM_BackwardCpp(SEXP xSEXP, SEXP ySEXP, SEXP offsetSEXP, SEXP indicesSEXP, SEXP numSEXP, SEXP interactionsSEXP, SEXP methodSEXP, SEXP mSEXP, SEXP LinkSEXP, SEXP DistSEXP, SEXP nthreadsSEXP, SEXP tolSEXP, SEXP maxitSEXP, SEXP keepSEXP, SEXP stepsSEXP, SEXP metricSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericMatrix >::type x(xSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type offset(offsetSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type indices(indicesSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type num(numSEXP);
    Rcpp::traits::input_parameter< IntegerMatrix >::type interactions(interactionsSEXP);
    Rcpp::traits::input_parameter< std::string >::type method(methodSEXP);
    Rcpp::traits::input_parameter< int >::type m(mSEXP);
    Rcpp::traits::input_parameter< std::string >::type Link(LinkSEXP);
    Rcpp::traits::input_parameter< std::string >::type Dist(DistSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type nthreads(nthreadsSEXP);
    Rcpp::traits::input_parameter< double >::type tol(tolSEXP);
    Rcpp::traits::input_parameter< int >::type maxit(maxitSEXP);
    Rcpp::traits::input_parameter< IntegerVector >::type keep(keepSEXP);
    Rcpp::traits::input_parameter< unsigned int >::type steps(stepsSEXP);
    Rcpp::traits::input_parameter< std::string >::type metric(metricSEXP);
    rcpp_result_gen = Rcpp::wrap(BackwardCpp(x, y, offset, indices, num, interactions, method, m, Link, Dist, nthreads, tol, maxit, keep, steps, metric));
    return rcpp_result_gen;
END_RCPP
}
// MakeTable
NumericMatrix MakeTable(NumericVector preds, NumericVector y, double cutoff);
RcppExport SEXP _BranchGLM_MakeTable(SEXP predsSEXP, SEXP ySEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type preds(predsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(MakeTable(preds, y, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// MakeTableFactor2
NumericMatrix MakeTableFactor2(NumericVector preds, CharacterVector y, CharacterVector levels, double cutoff);
RcppExport SEXP _BranchGLM_MakeTableFactor2(SEXP predsSEXP, SEXP ySEXP, SEXP levelsSEXP, SEXP cutoffSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type preds(predsSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type levels(levelsSEXP);
    Rcpp::traits::input_parameter< double >::type cutoff(cutoffSEXP);
    rcpp_result_gen = Rcpp::wrap(MakeTableFactor2(preds, y, levels, cutoff));
    return rcpp_result_gen;
END_RCPP
}
// CindexCpp
double CindexCpp(NumericVector preds, NumericVector y);
RcppExport SEXP _BranchGLM_CindexCpp(SEXP predsSEXP, SEXP ySEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type preds(predsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    rcpp_result_gen = Rcpp::wrap(CindexCpp(preds, y));
    return rcpp_result_gen;
END_RCPP
}
// CindexTrap
double CindexTrap(NumericVector Sens, NumericVector Spec);
RcppExport SEXP _BranchGLM_CindexTrap(SEXP SensSEXP, SEXP SpecSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type Sens(SensSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Spec(SpecSEXP);
    rcpp_result_gen = Rcpp::wrap(CindexTrap(Sens, Spec));
    return rcpp_result_gen;
END_RCPP
}
// ROCCpp
DataFrame ROCCpp(NumericVector preds, NumericVector y, NumericVector Cutoffs);
RcppExport SEXP _BranchGLM_ROCCpp(SEXP predsSEXP, SEXP ySEXP, SEXP CutoffsSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< NumericVector >::type preds(predsSEXP);
    Rcpp::traits::input_parameter< NumericVector >::type y(ySEXP);
    Rcpp::traits::input_parameter< NumericVector >::type Cutoffs(CutoffsSEXP);
    rcpp_result_gen = Rcpp::wrap(ROCCpp(preds, y, Cutoffs));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_BranchGLM_BranchAndBoundCpp", (DL_FUNC) &_BranchGLM_BranchAndBoundCpp, 19},
    {"_BranchGLM_BackwardBranchAndBoundCpp", (DL_FUNC) &_BranchGLM_BackwardBranchAndBoundCpp, 18},
    {"_BranchGLM_SwitchBranchAndBoundCpp", (DL_FUNC) &_BranchGLM_SwitchBranchAndBoundCpp, 18},
    {"_BranchGLM_BranchGLMfit", (DL_FUNC) &_BranchGLM_BranchGLMfit, 12},
    {"_BranchGLM_MetricIntervalCpp", (DL_FUNC) &_BranchGLM_MetricIntervalCpp, 19},
    {"_BranchGLM_ForwardCpp", (DL_FUNC) &_BranchGLM_ForwardCpp, 16},
    {"_BranchGLM_BackwardCpp", (DL_FUNC) &_BranchGLM_BackwardCpp, 16},
    {"_BranchGLM_MakeTable", (DL_FUNC) &_BranchGLM_MakeTable, 3},
    {"_BranchGLM_MakeTableFactor2", (DL_FUNC) &_BranchGLM_MakeTableFactor2, 4},
    {"_BranchGLM_CindexCpp", (DL_FUNC) &_BranchGLM_CindexCpp, 2},
    {"_BranchGLM_CindexTrap", (DL_FUNC) &_BranchGLM_CindexTrap, 2},
    {"_BranchGLM_ROCCpp", (DL_FUNC) &_BranchGLM_ROCCpp, 3},
    {NULL, NULL, 0}
};

RcppExport void R_init_BranchGLM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
