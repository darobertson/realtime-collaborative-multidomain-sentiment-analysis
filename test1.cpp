// [[Rcpp::depends(RcppArmadillo)]]
#include <RcppArmadillo.h>
using namespace Rcpp ;

arma::mat matmult_sp(const arma::sp_mat X, const arma::mat Y){
    arma::mat ret = X * Y;
    return ret;
};
arma::mat matmult_dense(const arma::mat X, const arma::mat Y){
    arma::mat ret = X * Y;
    return ret;
};
arma::mat matmult_ind(const SEXP Xr, const arma::mat Y){
    // pre-multiplication with index matrix is a permutation of Y's rows: 
    arma::uvec perm =  as<S4>(Xr).slot("perm");
    arma::mat ret = Y.rows(perm - 1);
    return ret;
};

//[[Rcpp::export]]
arma::mat matmult_cpp(SEXP Xr, const arma::mat Y) {
    if (Rf_isS4(Xr)) {
        if(Rf_inherits(Xr, "dgCMatrix")) {
            return matmult_sp(as<arma::sp_mat>(Xr), Y) ;
        } ;
        if(Rf_inherits(Xr, "indMatrix")) {
            return matmult_ind(Xr, Y) ; 
        } ;
        stop("unknown class of Xr") ;
    } else {
        return matmult_dense(as<arma::mat>(Xr), Y) ;
    } 
}