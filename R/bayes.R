#' Compute Bayes estimator
#'
#' Compute Bayes estimator of a vector of matrices of weights, as defined in Madigan and York (1997).
#'
#' @usage bayesEstimator(weights, N)
#'
#' @param weights Matrix of posterior weights for the population size. Each row should be a (possibly un-normalized) posterior distribution on the parameter `N`, as in the result of `bma.cr`.
#' @param N Population sizes under consideration. Each row of `weights` represents a distribution on `N`.
#'
#' @return Vector of Bayes estimators corresponding to each row of `weights`, if it is a matrix.
#'
#' @examples
#' delta <- .5
#' Y <- c(0,27,37,19,4,4,1,1,97,22,37,25,2,1,3,5,83,36,34,18,3,5,0,2,30,5,23,8,0,3,0,2)
#' Y <- array(Y, dim=c(2,2,2,2,2))
#' Nmissing <- 1:300
#' N <- Nmissing + sum(Y)
#' data(graphs5)
#' weights <- bma.cr(Y,  Nmissing, delta, graphs5)
#'
#' # Full Bayes estimator
#' bayesEstimator(colSums(weights), N)
#'
#' # Model-wise Bayes estimators
#' bayesEstimator(weights, N)
#'
#' @export
bayesEstimator <- function(weights, N) {
  UseMethod("bayesEstimator", weights)
}

.bayesEstimator <- function(weights, N) {
  mean(weights/N)/mean(weights/(N*N))
}

#' @export
bayesEstimator.default <- function(weights, N) {
  assert(is.numeric(weights),
         is.numeric(N),
         length(weights) == length(N))

  .bayesEstimator(weights, N)
}

#' @export
bayesEstimator.matrix <- function(weights, N) {
  assert(is.matrix(weights),
         is.numeric(N),
         ncol(weights) == length(N))

  sapply(1:nrow(weights), function(i) .bayesEstimator(weights[i,,drop=TRUE], N))
}
