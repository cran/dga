#' @export
bma.cr <- function(Y, Nmissing, delta, graphs,
                   logprior = NULL,
                   log.prior.model.weights = NULL) {
  if (is.null(logprior)) {
    logprior <- -log(sum(Y) + Nmissing)
  }

  Y[1] <- 0
  p <- length(dim(Y))
  alpha <- rep(delta, length(Y))

  # Precomputations
  compMat <- MakeCompMatrix(p, delta, Y, Nmissing)
  D <- lgamma(sum(alpha)) - lgamma(Nmissing + sum(Y) + sum(alpha))
  multinomialCoefficient <- lgamma(Nmissing + sum(Y) + 1) - sum(lgamma(Y[-1] + 1)) - lgamma(Nmissing + 1)

  # Compute log posterior for all models
  weights <- computeLogPostProbs(compMat, graphs, D, p, length(Nmissing))
  rowAdd(weights, multinomialCoefficient)
  rowAdd(weights, logprior)
  if (!is.null(log.prior.model.weights)) colAdd(weights, log.prior.model.weights)

  # Normalization
  expNormalize(weights)

  return(weights)
}
