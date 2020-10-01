#include <Rcpp.h>
using namespace Rcpp;

NumericVector colSumsSub(const NumericMatrix& mat,
                         const IntegerVector& rowIDs) {
  int ncols = mat.ncol();
  NumericVector sum(ncols);

  int i;
  for (IntegerVector::const_iterator it = rowIDs.begin(); it != rowIDs.end(); ++it) {
    i = *it - 1;
    sum += mat(i, _);
  }

  return sum;
}

// [[Rcpp::export]]
NumericVector computeML(const NumericMatrix& compMat,
                        const IntegerVector& decC,
                        const IntegerVector& decS,
                        const NumericVector& denominator) {

  double nsubgraphs = decC.size() - decS.size();

  return colSumsSub(compMat, decC)
    - colSumsSub(compMat, decS)
    + nsubgraphs*denominator;
}

// [[Rcpp::export]]
void rowAdd(NumericMatrix& mat,
            const NumericVector& vect) {

  double v;
  for (int j = 0; j < mat.ncol(); j++) {
    v = vect(j);
    for (int i = 0; i < mat.nrow(); i++) {
      mat(i,j) += v;
    }
  }
}
