// Generated by using Rcpp::compileAttributes() -> do not edit by hand
// Generator token: 10BE3573-1514-4C36-9D1C-5A225CD40393

#include <Rcpp.h>

using namespace Rcpp;

#ifdef RCPP_USE_GLOBAL_ROSTREAM
Rcpp::Rostream<true>&  Rcpp::Rcout = Rcpp::Rcpp_cout_get();
Rcpp::Rostream<false>& Rcpp::Rcerr = Rcpp::Rcpp_cerr_get();
#endif

// jsd
DataFrame jsd(DataFrame text, CharacterVector group_list, CharacterVector word_list, String group, String word, String n);
RcppExport SEXP _dhmeasures_jsd(SEXP textSEXP, SEXP group_listSEXP, SEXP word_listSEXP, SEXP groupSEXP, SEXP wordSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type text(textSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type group_list(group_listSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type word_list(word_listSEXP);
    Rcpp::traits::input_parameter< String >::type group(groupSEXP);
    Rcpp::traits::input_parameter< String >::type word(wordSEXP);
    Rcpp::traits::input_parameter< String >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(jsd(text, group_list, word_list, group, word, n));
    return rcpp_result_gen;
END_RCPP
}
// log_likelihood
DataFrame log_likelihood(DataFrame text, CharacterVector group_list, CharacterVector word_list, String group, String word, String n);
RcppExport SEXP _dhmeasures_log_likelihood(SEXP textSEXP, SEXP group_listSEXP, SEXP word_listSEXP, SEXP groupSEXP, SEXP wordSEXP, SEXP nSEXP) {
BEGIN_RCPP
    Rcpp::RObject rcpp_result_gen;
    Rcpp::RNGScope rcpp_rngScope_gen;
    Rcpp::traits::input_parameter< DataFrame >::type text(textSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type group_list(group_listSEXP);
    Rcpp::traits::input_parameter< CharacterVector >::type word_list(word_listSEXP);
    Rcpp::traits::input_parameter< String >::type group(groupSEXP);
    Rcpp::traits::input_parameter< String >::type word(wordSEXP);
    Rcpp::traits::input_parameter< String >::type n(nSEXP);
    rcpp_result_gen = Rcpp::wrap(log_likelihood(text, group_list, word_list, group, word, n));
    return rcpp_result_gen;
END_RCPP
}

static const R_CallMethodDef CallEntries[] = {
    {"_dhmeasures_jsd", (DL_FUNC) &_dhmeasures_jsd, 6},
    {"_dhmeasures_log_likelihood", (DL_FUNC) &_dhmeasures_log_likelihood, 6},
    {NULL, NULL, 0}
};

RcppExport void R_init_dhmeasures(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
