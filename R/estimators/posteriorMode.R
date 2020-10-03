#' @export
posteriorMode <- function(weights, N) {
  UseMethod("posteriorMode", weights)
}

.posteriorMode <- function(weights, N) {
  N[which.max(weights)]
}

posteriorMode.default <- function(weights, N) {
  assert(is.numeric(weights),
         is.numeric(N),
         length(weights) == length(N))

  .posteriorMode(weights, N)
}

posteriorMode.matrix <- function(weights, N) {
  assert(is.matrix(weights),
         is.numeric(N),
         ncol(weights) == length(N))

  sapply(1:nrow(weights), function(i) .posteriorMode(weights[i,,drop=TRUE], N))
}
