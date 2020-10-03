bayesEstimator <- function(weights, N) {
  UseMethod("bayesEstimator", weights)
}

bayesEstimator.numeric <- function(weights, N) {
  assert(length(weights) == length(N))

  mean(weights/N)/mean(weights/(N*N))
}

bayesEstimator.matrix <- function(weights, N) {
  assert(ncol(weights) == length(N))

  sapply(1:nrow(weights), function(i) bayesEstimator.numeric(weights[i,,drop=TRUE], N))
}

