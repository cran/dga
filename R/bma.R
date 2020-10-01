#' @export
bma.cr <- function(Y, Nmissing, delta, graphs,
                   logprior = NULL,
                   log.prior.model.weights = 0) {
  if (is.null(logprior)) {
    logprior <- -log(sum(Y) + Nmissing)
  }

  Y[1] <- 0
  p <- length(dim(Y))
  alpha <- rep(delta, length(Y))

  # Matrix of log posterior probabilities
  weights <- array(dim = c(length(graphs), length(Nmissing)))

  # Precomputations
  compMat <- MakeCompMatrix(p, delta, Y, Nmissing)
  D <- lgamma(sum(alpha)) - lgamma(Nmissing + sum(Y) + sum(alpha))
  multinomialCoefficient <- lgamma(Nmissing + sum(Y) + 1) - sum(lgamma(Y[-1] + 1)) - lgamma(Nmissing + 1)

  # Compute log posterior for all models
  for (j in seq_along(graphs)) {
    graph = graphs[[j]]
    decC <- sapply(graph$C, function(g) sum(2^(p - g)))
    decS <- as.numeric(sapply(graph$S, function(g) sum(2^(p - g)))) # as.numeric coerces empty list to numeric
    weights[j, ] <- computeML(compMat, decC, decS, D)
  }
  rowAdd(weights, multinomialCoefficient)
  rowAdd(weights, logprior)
  weights <- weights + log.prior.model.weights

  # Normalization
  weights <- exp(weights - max(weights))
  weights <- weights / sum(weights)

  return(weights)
}
