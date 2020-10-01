#include <RcppArmadillo.h>
using namespace Rcpp;

// [[Rcpp::export]]
arma::rowvec colSumsSub(const arma::mat& mat,
                         const arma::uvec& rowIDs) {

  return arma::sum(mat.rows(rowIDs-1), 0);
}

// [[Rcpp::export]]
void computeML(arma::mat& inPlace,
                        int j,
                        const arma::mat& compMat,
                        const arma::uvec& decC,
                        const arma::uvec& decS,
                        const arma::rowvec& denominator) {

  double nsubgraphs = decC.n_elem - decS.n_elem;

  inPlace.row(j-1) = colSumsSub(compMat, decC)
    - colSumsSub(compMat, decS)
    + nsubgraphs*denominator;
}

// [[Rcpp::export]]
void rowAdd(arma::mat& mat,
            const arma::rowvec& v) {

  mat.each_row() += v;
}

// [[Rcpp::export]]
void colAdd(arma::mat& mat,
            const arma::colvec& v) {

  mat.each_col() += v;
}

// [[Rcpp::export]]
void expNormalize(arma::mat& mat) {
  mat = exp(mat);
  mat = mat/sum(sum(mat));
}


