#' @export
bayesEstimator <- function(weights, N) {
  UseMethod("bayesEstimator", weights)
}

.bayesEstimator <- function(weights, N) {
  mean(weights/N)/mean(weights/(N*N))
}

bayesEstimator.default <- function(weights, N) {
  assert(is.numeric(weights),
         is.numeric(N),
         length(weights) == length(N))

  .bayesEstimator(weights, N)
}

bayesEstimator.matrix <- function(weights, N) {
  assert(is.matrix(weights),
         is.numeric(N),
         ncol(weights) == length(N))

  sapply(1:nrow(weights), function(i) .bayesEstimator(weights[i,,drop=TRUE], N))
}
