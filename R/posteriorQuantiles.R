
#' @export
posteriorQuantiles <- function(weights, N, levels=c(0.025, 0.975), normalize=TRUE) {
  UseMethod("posteriorQuantiles", weights)
}

.posteriorQuantiles <- function(weights, N, levels=c(0.025, 0.975), normalize=TRUE) {
  if (normalize) weights = weights/sum(weights)

  cs = cumsum(weights)
  sapply(levels, function(level) N[which(cs > level)[1]])
}

posteriorQuantiles.default <- function(weights, N, ...) {
  assert(is.numeric(weights),
         is.numeric(N))

  .posteriorQuantiles(weights, N, ...)
}

posteriorQuantiles.matrix <- function(weights, N, levels=c(0.025, 0.975), normalize=TRUE) {
  assert(is.matrix(weights),
         is.numeric(N),
         ncol(weights) == length(N))

  if (normalize) weights = weights/rowSums(weights)

  m = sapply(1:nrow(weights), function(i) .posteriorQuantiles(weights[i,,drop=FALSE], N, levels, normalize=FALSE))

  rownames(m) = levels
  return(m)
}
